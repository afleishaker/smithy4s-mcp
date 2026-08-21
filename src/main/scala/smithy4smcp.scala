package app

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all._
import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec._
import com.github.plokhotnyuk.jsoniter_scala.core._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax._
import jsonrpclib.CallId
import jsonrpclib.InputMessage.NotificationMessage
import jsonrpclib.InputMessage.RequestMessage
import jsonrpclib.JsonRpcPayload
import jsonrpclib.JsonRpcRequest
import jsonrpclib.Message
import jsonrpclib.OutputMessage.ResponseMessage
import jsonrpclib.Payload
import jsonrpclib.ProtocolError
import jsonrpclib.fs2._
import jsonrpclib.smithy4sinterop.CirceJsonCodec
import jsonrpclib.smithy4sinterop.ClientStub
import jsonrpclib.smithy4sinterop.ServerEndpoints
import mcptraits.McpClientApi
import mcptraits.McpClientApiGen
import mcptraits.McpServerApi
import mcptraits.McpServerApiGen
import mcptraits.McpTool
import modelcontextprotocol.CallToolResult
import modelcontextprotocol.ClientCapabilities
import modelcontextprotocol.ContentBlock
import modelcontextprotocol.ContentBlock.TextCase
import modelcontextprotocol.Cursor
import modelcontextprotocol.ElicitFormSchema
import modelcontextprotocol.ElicitRequestFormParams
import modelcontextprotocol.ElicitRequestParams
import modelcontextprotocol.Implementation
import modelcontextprotocol.InitializeResult
import modelcontextprotocol.ListToolsResult
import modelcontextprotocol.ServerCapabilities
import modelcontextprotocol.TaskMetadata
import modelcontextprotocol.Tool
import modelcontextprotocol.ToolAnnotations
import modelcontextprotocol.ToolSchema
import modelcontextprotocol.ToolsCapability
import org.http4s.Header
import org.http4s.Method
import org.http4s.Request
import org.http4s.ServerSentEvent
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.headers.Accept
import org.http4s.headers.`Content-Type`
import org.typelevel.ci.CIString
import smithy.api.Readonly
import smithy4s.Bijection
import smithy4s.Document
import smithy4s.Endpoint
import smithy4s.Hints
import smithy4s.Service
import smithy4s.ShapeId
import smithy4s.kinds.FunctorAlgebra
import smithy4s.schema.Alt
import smithy4s.schema.CollectionTag
import smithy4s.schema.EnumTag
import smithy4s.schema.EnumTag.ClosedIntEnum
import smithy4s.schema.EnumTag.ClosedStringEnum
import smithy4s.schema.EnumValue
import smithy4s.schema.Field
import smithy4s.schema.Primitive
import smithy4s.schema.Schema
import smithy4s.schema.Schema.StructSchema
import smithy4s.schema.Schema.UnionSchema
import smithy4s.schema.SchemaVisitor
import smithy4s.~>

import scala.collection.immutable.ListMap

import McpBuilder.internal._

object McpBuilder {

  def server[Alg[_[_, _, _, _, _]]](
    impl: FunctorAlgebra[Alg, IO]
  )(
    implicit service: Service[Alg]
  ): McpServerApi[IO] =
    new McpServerApi[IO] {
      def ping(): IO[Unit] = IO.unit

      val allMyMonkeysCompiled: ListMap[String, CompiledTool] = {
        val fk = service.toPolyFunction(impl)

        // Scala 2 skolemizes the wildcards of `service.endpoints` independently at each use site,
        // so the endpoint's type parameters have to be opened by an explicitly parameterized
        // method before `decodeIn`, `e.wrap` and `encodeOut` can be seen to line up.
        def compileOne[I, E, O, SI, SO](e: service.Endpoint[I, E, O, SI, SO]): CompiledTool = {
          val toolHint = e
            .hints
            .get[McpTool]
            .getOrElse(
              sys.error(s"Endpoint ${e.id} is not a tool, we can't compile it")
            )

          val decodeIn = Document.Decoder.fromSchema(e.input)
          val encodeOut = Document.Encoder.fromSchema(e.output)

          CompiledTool(
            Tool(
              name = toolName(toolHint, e),
              inputSchema = deriveSchema(e.input),
              outputSchema = Some(deriveSchema(e.output)),
              annotations = Some(
                ToolAnnotations(
                  readOnlyHint = Some(e.hints.has[Readonly]),
                  idempotentHint = Some(e.hints.has[smithy.api.Idempotent]),
                )
              ),
            ),
            impl = { doc =>
              decodeIn
                .decode(doc)
                .liftTo[IO]
                .map(i => e.wrap(i))
                .flatMap(op => fk(op))
                .map(o => encodeOut.encode(o))
            },
          )
        }

        service
          .endpoints
          .filter(_.hints.has[McpTool])
          .map(e => compileOne(e))
          .map(ct => ct.tool.name -> ct)
          .to(ListMap)
      }

      def initialize(
        protocolVersion: String,
        capabilities: ClientCapabilities,
        clientInfo: Implementation,
        _meta: Option[Map[String, Document]],
      ): IO[InitializeResult] =
        printErr("default initialize called") *>
          IO.pure(
            InitializeResult(
              protocolVersion = "2025-11-25",
              capabilities = ServerCapabilities(
                tools = Some(ToolsCapability())
              ),
              serverInfo = Implementation(
                name = "mcp-notes-server",
                version = "0.0.0",
              ),
            )
          )

      def listTools(cursor: Option[Cursor], _meta: Option[Map[String, Document]])
        : IO[ListToolsResult] =
        printErr("listTools called") *> IO.pure(
          ListToolsResult(
            tools = allMyMonkeysCompiled.values.map(_.tool).toList
          )
        )

      def callTool(
        name: String,
        arguments: Option[Document],
        task: Option[TaskMetadata],
        _meta: Option[Map[String, Document]],
      ): IO[CallToolResult] =
        printErr(s"callTool called with name: $name and arguments: $arguments") *>
          allMyMonkeysCompiled
            .get(name)
            .liftTo[IO](new Exception(s"Tool $name not found"))
            .flatMap { ct =>
              val argsDoc = arguments.getOrElse(Document.obj())

              ct.impl(argsDoc).map { outDoc =>
                CallToolResult(
                  content = Nil,
                  structuredContent = Some(outDoc),
                )
              }
            }
    }

  object internal {

    case class CompiledTool(
      tool: Tool,
      impl: Document => IO[Document],
    )

    /** Const type constructor: stands in for Scala 3's `[_] =>> JsonSchema`, so that
      * `SchemaVisitor` can be instantiated at a type that ignores the schema's type parameter.
      */
    type ConstSchema[A] = JsonSchema

    sealed trait JsonSchema {

      def asDocument: Document =
        this match {
          case JsonSchema.ObjectSchema(properties, required) =>
            Document.obj(
              "type" -> Document.fromString("object"),
              "properties" -> Document.obj(
                properties.map { case (k, v) => k -> v.asDocument }.toSeq
              ),
              "required" -> Document.array(required.map(Document.fromString)),
            )
          case JsonSchema.NumberSchema         => Document.obj("type" -> Document.fromString("number"))
          case JsonSchema.StringSchema         => Document.obj("type" -> Document.fromString("string"))
          case JsonSchema.AnyOfSchema(options) =>
            Document.obj(
              "anyOf" -> Document.array(options.map(_.asDocument))
            )
          case JsonSchema.ListSchema(itemSchema) =>
            Document.obj(
              "type" -> Document.fromString("array"),
              "items" -> itemSchema.asDocument,
            )
          case JsonSchema.EnumSchema(options) =>
            Document.obj(
              "type" -> Document.fromString("string"),
              "enum" -> Document.array(options.map(Document.fromString)),
            )
          case JsonSchema.IntEnumSchema(options) =>
            Document.obj(
              "type" -> Document.fromString("integer"),
              "enum" -> Document.array(options.map(Document.fromInt)),
            )
        }

    }

    object JsonSchema {
      final case class ObjectSchema(properties: Map[String, JsonSchema], required: List[String])
        extends JsonSchema
      final case class AnyOfSchema(options: List[JsonSchema]) extends JsonSchema
      case object NumberSchema extends JsonSchema
      case object StringSchema extends JsonSchema
      final case class ListSchema(itemSchema: JsonSchema) extends JsonSchema
      final case class EnumSchema(options: List[String]) extends JsonSchema
      final case class IntEnumSchema(options: List[Int]) extends JsonSchema
    }

    object SchemaDerivation extends SchemaVisitor.Default[ConstSchema] {
      def default[A]: JsonSchema = ???

      override def union[U](
        shapeId: ShapeId,
        hints: Hints,
        alternatives: Vector[Alt[U, _]],
        dispatch: Alt.Dispatcher[U],
      ): JsonSchema = JsonSchema.AnyOfSchema(
        alternatives.toList.map(alt => alt.schema.compile(this))
      )

      override def primitive[P](shapeId: ShapeId, hints: Hints, tag: Primitive[P]): JsonSchema =
        tag match {
          case Primitive.PInt    => JsonSchema.NumberSchema
          case Primitive.PString => JsonSchema.StringSchema
          case _                 => ???
        }

      override def enumeration[E](
        shapeId: ShapeId,
        hints: Hints,
        tag: EnumTag[E],
        values: List[EnumValue[E]],
        total: E => EnumValue[E],
      ): JsonSchema =
        tag match {
          case ClosedIntEnum    => JsonSchema.IntEnumSchema(values.map(_.intValue))
          case ClosedStringEnum => JsonSchema.EnumSchema(values.map(_.stringValue))
          case _                => sys.error("Open enums are not supported")
        }

      override def biject[A, B](schema: Schema[A], bijection: Bijection[A, B]): JsonSchema = schema
        .compile(
          this
        ) // just compile the underlying schema, the bijection doesn't change the JSON schema

      override def option[A](schema: Schema[A]): JsonSchema = schema.compile(
        this
      ) // optionality handled on struct level

      override def collection[C[_], A](
        shapeId: ShapeId,
        hints: Hints,
        tag: CollectionTag[C],
        member: Schema[A],
      ): JsonSchema = JsonSchema.ListSchema(member.compile(this))

      override def struct[S](
        shapeId: ShapeId,
        hints: Hints,
        fields: Vector[Field[S, _]],
        make: IndexedSeq[Any] => S,
      ): JsonSchema = JsonSchema.ObjectSchema(
        properties = fields.map(f => f.label -> f.schema.compile(this)).toMap,
        required = fields.filter(_.isRequired).map(_.label).toList,
      )

    }

    def deriveSchema[A](implicit schema: Schema[A]): ToolSchema =
      schema.compile(SchemaDerivation) match {
        case JsonSchema.ObjectSchema(properties, required) =>
          ToolSchema(
            _type = "object",
            properties = Some(
              Document.obj(properties.map { case (k, v) => k -> v.asDocument }.toSeq)
            ),
            required = Some(required),
          )
        case _ => sys.error("Only object schemas are supported on the top level")
      }

  }

  def remoteServerStub[Alg[_[_, _, _, _, _]]](
    service: Service[Alg]
  )(
    implicit rawServer: McpServerApi[IO]
  ): service.Impl[IO] = service.impl {
    new service.FunctorEndpointCompiler[IO] {
      def apply[I, E, O, SI, SO](e: service.Endpoint[I, E, O, SI, SO]): I => IO[O] = {
        val toolHint = e
          .hints
          .get[McpTool]
          .getOrElse(
            sys.error(
              s"Endpoint ${e.id} is not a tool, we can't derive a client for it"
            )
          )

        val inputEncoder = Document.Encoder.fromSchema(e.input)
        val resultDecoder = Document.Decoder.fromSchema(JsonPayloadTransformation(e.output))

        i =>
          rawServer
            .callTool(name = toolName(toolHint, e), arguments = inputEncoder.encode(i).some)
            .flatMap { result =>
              result.structuredContent match {
                case Some(content) => resultDecoder.decode(content).liftTo[IO]

                // Best-effort JSON decoding from unstructured content.
                // The github MCP doesn't use structured responses, for example, so we shouldn't be typing them
                // but I figured for the example's sake there's no reason not to at least try.
                case None =>
                  val docs = result
                    .content
                    .map {
                      case TextCase(text) => text.text
                      case other          =>
                        sys.error(
                          s"Only text content blocks are supported in this context. Got ${ContentBlock.schema.asInstanceOf[UnionSchema[_]].alternatives(other.$ordinal).label}."
                        )
                    }
                    .map { text =>
                      smithy4s.json.Json.readDocument(text).getOrElse(Document.fromString(text))
                    }

                  docs match {
                    case one :: Nil => resultDecoder.decode(one).liftTo[IO]
                    case _          => resultDecoder.decode(Document.array(docs)).liftTo[IO]
                  }
              }
            }
      }
    }
  }

  // TODO: publish this from jsonrpclib
  private object JsonPayloadTransformation extends (Schema ~> Schema) {

    def apply[A0](fa: Schema[A0]): Schema[A0] =
      fa match {
        case s: StructSchema[_] =>
          val struct = s.asInstanceOf[StructSchema[A0]]

          struct
            .fields
            .collectFirst { case field if field.hints.has[JsonRpcPayload] => field }
            .map(field => payloadBijection(struct, field.asInstanceOf[Field[A0, Any]]))
            .getOrElse(fa)

        case _ => fa
      }

    private def payloadBijection[S](struct: StructSchema[S], field: Field[S, Any]): Schema[S] =
      field.schema.biject[S]((a: Any) => struct.make(Vector(a)))(field.get)

  }

  private def toolName[Op[_, _, _, _, _]](toolHint: McpTool, e: Endpoint[Op, _, _, _, _, _])
    : String = toolHint.name.getOrElse(e.id.name)

  def clientStub[Alg[_[_, _, _, _, _]]](
    service: Service[Alg]
  )(
    implicit rawClient: McpClientApi[IO]
  ): service.Impl[IO] = service.impl {
    new service.FunctorEndpointCompiler[IO] {
      def apply[I, E, O, SI, SO](fa: service.Endpoint[I, E, O, SI, SO]): I => IO[O] = {

        val messageFinder: Option[I => String] =
          fa.input match {
            case s: StructSchema[_] =>
              s.asInstanceOf[StructSchema[I]]
                .fields
                .find(_.label == "message")
                .map(field => messageExtractor(field.asInstanceOf[Field[I, Any]]))

            case _ => None
          }

        val inputToMessage: I => String = messageFinder.getOrElse(
          (_: I) => s"Server is asking (${fa.id.name})"
        )

        val requestedSchema = {
          val compiled = deriveSchema(fa.output)
          ElicitFormSchema(
            _type = "object",
            properties = compiled.properties.get,
            required = compiled.required,
          )
        }

        val resultDecoder = Document.Decoder.fromSchema(fa.output)

        { i =>
          rawClient
            .elicitation(
              ElicitRequestParams.form(
                form = ElicitRequestFormParams(
                  message = inputToMessage(i),
                  requestedSchema = requestedSchema,
                )
              )
            )
            .flatMap(result => resultDecoder.decode(result.content.get).liftTo[IO])
        }
      }
    }
  }

  private def messageExtractor[S, A](field: Field[S, A]): S => String = {
    val toDoc = Document.Encoder.fromSchema(field.schema)

    field
      .get
      .andThen { a =>
        toDoc.encode(a) match {
          case Document.DString(s) => s
          case _                   => sys.error("Expected the 'message' field to be a string")
        }
      }
  }

  def httpClient[Remote[_[_, _, _, _, _]]](
    service: Service[Remote],
    rawClient: Client[IO],
    baseUrl: Uri,
  ): IO[service.Impl[IO]] = (IO.ref(0L), IO.ref(none[String]))
    .tupled
    .map { case (callIdRef, sessionIdRef) =>
      service.impl(new service.FunctorEndpointCompiler[IO] {
        def apply[I, E, O, SI, SO](fa: service.Endpoint[I, E, O, SI, SO]): I => IO[O] = {
          val encodeIn = CirceJsonCodec.Encoder.fromSchema(fa.input)

          val decodeOut = CirceJsonCodec.Decoder.fromSchema(fa.output)

          { in =>
            (callIdRef.getAndUpdate(_ + 1).map(CallId.NumberId(_)), sessionIdRef.get)
              .tupled
              .flatMap { case (callId, sessionIdOpt) =>
                rawClient
                  .run(
                    Request[IO](method = Method.POST, uri = baseUrl)
                      .withBodyStream(
                        fs2
                          .Stream
                          .emits(
                            {
                              RequestMessage(
                                method = fa.hints.get[JsonRpcRequest].get.value,
                                callId = callId,
                                params = Some(Payload(encodeIn(in))),
                              ): Message
                            }
                              .asJson
                              .noSpaces
                              .getBytes()
                          )
                      )
                      .withContentType(
                        org
                          .http4s
                          .headers
                          .`Content-Type`(org.http4s.MediaType.application.json)
                      )
                      .withHeaders(
                        Accept(
                          org.http4s.MediaType.application.json,
                          org.http4s.MediaType.`text/event-stream`,
                        ),
                        sessionIdOpt.map(sid => Header.Raw(CIString("mcp-session-id"), sid)),
                      )
                  )
                  .use { response =>
                    def decodeResponseMessage(txt: String) = io
                      .circe
                      .parser
                      .decode[Message](txt)
                      .liftTo[IO]
                      .adaptError { case e =>
                        new RuntimeException(
                          s"Failed to decode JSON-RPC message: ${e.getMessage}, from ${txt}",
                          e,
                        )
                      }
                      .map { case rm: ResponseMessage => rm }

                    def completeDecoding(rm: ResponseMessage) = decodeOut
                      .decodeJson(rm.data.data)
                      .liftTo[IO]
                      .adaptError { case e =>
                        new RuntimeException(
                          s"Failed to decode response for call $callId: ${e.getMessage}",
                          e,
                        )
                      }

                    val isStream = response
                      .headers
                      .get[`Content-Type`]
                      .exists { ct =>
                        ct.mediaType == org.http4s.MediaType.`text/event-stream`
                      }

                    val handleStream = response
                      .body
                      .through(ServerSentEvent.decoder[IO])
                      // .debug("SSE: " + _)
                      .collect {
                        case sse if sse.eventType.contains("message") && sse.data.isDefined =>
                          sse.data.get
                      }
                      .evalMap(decodeResponseMessage)
                      .collectFirst { case rm if rm.callId == callId => rm }
                      .compile
                      .toList
                      .map(_.head)

                    val handleJson = response
                      .bodyText
                      .compile
                      .string
                      .flatMap(decodeResponseMessage)

                    response
                      .headers
                      .get(CIString("mcp-session-id"))
                      .map(_.head.value)
                      .traverse_(sid => sessionIdRef.set(Some(sid))) *>
                      {
                        if (isStream) handleStream
                        else
                          handleJson
                      }
                        .flatMap(completeDecoding)
                        .adaptError { case e =>
                          new RuntimeException(s"HTTP request failed: ${e.getMessage}", e)
                        }
                  }
              }
          }
        }
      })
    }

}

object interop {

  // Scala 2 cannot infer `Remote` from the argument type of the `srv` function, so both
  // algebras are named explicitly here.
  def startServer(srv: McpClientApi[IO] => McpServerApi[IO]): Resource[IO, McpClientApi[IO]] =
    startGen[McpServerApiGen, McpClientApiGen](srv, fs2.io.stdin[IO](512), fs2.io.stdout[IO])

  def startClient(c: McpServerApi[IO] => McpClientApi[IO], process: fs2.io.process.Process[IO])
    : Resource[IO, McpServerApi[IO]] =
    startGen[McpClientApiGen, McpServerApiGen](c, process.stdout, process.stdin)

  def startGen[Local[_[_, _, _, _, _]], Remote[_[_, _, _, _, _]]](
    srv: FunctorAlgebra[Remote, IO] => FunctorAlgebra[Local, IO],
    input: fs2.Stream[IO, Byte],
    output: fs2.Pipe[IO, Byte, Nothing],
  )(
    implicit localService: Service[Local],
    remoteService: Service[Remote],
  ): Resource[IO, FunctorAlgebra[Remote, IO]] = FS2Channel
    .resource[IO](cancelTemplate = Some(cancelEndpoint))
    .flatMap { channel =>
      ClientStub(remoteService, channel).liftTo[IO].toResource.flatMap { client =>
        ServerEndpoints(srv(client))
          .liftTo[IO]
          .toResource
          .flatMap { se =>
            channel.withEndpoints(se)
          }
          .flatMap { channel =>
            input
              // .observe(_.through(Files[IO].writeAll(fs2.io.file.Path("input.log"))))
              .through {
                _.through(fs2.text.utf8.decode[IO])
                  .through(fs2.text.lines[IO])
                  .map { line =>
                    Payload(readFromArray[Json](line.getBytes()))
                  }
                  .map { payload =>
                    Decoder[Message]
                      .apply(HCursor.fromJson(payload.data))
                      .left
                      .map(e => ProtocolError.ParseError(e.getMessage))
                      .map {
                        // workaround for some clients (like Claude Code) sending requests/notifications with no body.
                        case rm: RequestMessage if rm.params.isEmpty =>
                          rm.copy(params = Some(Payload(Json.obj())))
                        case nm: NotificationMessage if nm.params.isEmpty =>
                          nm.copy(params = Some(Payload(Json.obj())))
                        case other => other
                      }
                  }
              }
              // .observe(
              // _.map(_.toString).through(Files[IO].writeUtf8(fs2.io.file.Path("decoded.log")))
              // )
              .through(channel.inputOrBounce)
              .compile
              .drain
              .background &> (
              channel
                .output
                .through(encode)
                // .observe(_.through(Files[IO].writeAll(fs2.io.file.Path("output.log"))))
                .through(output)
                .compile
                .drain
                .background
                .as(client)
            )
          }
      }
    }

  // Reserving a method for cancelation.
  val cancelEndpoint = CancelTemplate.make[CallId]("notifications/cancelled", identity, identity)

  val decode: fs2.Pipe[IO, Byte, Either[ProtocolError, Message]] =
    _.through(fs2.text.utf8.decode[IO])
      .through(fs2.text.lines[IO])
      .map { line =>
        Payload(readFromArray[Json](line.getBytes()))
      }
      .map { payload =>
        Decoder[Message]
          .apply(HCursor.fromJson(payload.data))
          .left
          .map(e => ProtocolError.ParseError(e.getMessage))
      }

  val encode: fs2.Pipe[IO, Message, Byte] =
    _.map(Encoder[Message].apply(_).noSpaces + "\n").through(fs2.text.utf8.encode[IO])

}
