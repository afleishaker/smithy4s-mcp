package app

import cats.effect.IO
import cats.effect.IOApp
import mcptraits.McpServerApi
import modelcontextprotocol.CallToolResult
import modelcontextprotocol.ClientCapabilities
import modelcontextprotocol.Cursor
import modelcontextprotocol.Implementation
import modelcontextprotocol.InitializeResult
import modelcontextprotocol.ListToolsResult
import modelcontextprotocol.TaskMetadata
import my.server.AdderOutput
import my.server.AskNameOutput
import my.server.Character
import my.server.CharacterType
import my.server.ListCharactersOutput
import my.server.MyClient
import my.server.MyServer
import my.server.Name
import smithy4s.Document

object main extends IOApp.Simple {

  def run: IO[Unit] = {

    def myTools(
      getClientCapabilities: IO[ClientCapabilities]
    )(
      implicit client: MyClient[IO]
    ): MyServer[IO] =
      new MyServer[IO] {
        def adder(a: Int, b: Option[Int]): IO[AdderOutput] =
          for {
            clientCaps <- getClientCapabilities
            _ <- printErr(s"Caps: $clientCaps")
            name <-
              if (clientCaps.elicitation.isDefined)
                client.askName("say my name")
              else
                IO.pure(AskNameOutput("default"))
            _ <- printErr(s"Hello, $name! Adding $a and ${b.getOrElse(0)}")
          } yield AdderOutput(
            result = a + b.getOrElse(0),
            Some(s"You're goddamn right, ${name.name}"),
          )

        def listCharacters(): IO[ListCharactersOutput] =

          IO.pure {
            ListCharactersOutput {
              List("walter", "mike")
                .map(
                  Name(_)
                )
                .map(Character(_, CharacterType.BAD))
            }
          }
      }

    printErr("Starting server") *>
      IO.ref(Option.empty[ClientCapabilities]).flatMap { clientCaps =>
        interop
          .startServer { rawClient =>
            val impl = McpBuilder.server(
              myTools(clientCaps.get.map(_.getOrElse(ClientCapabilities())))(
                McpBuilder.clientStub(MyClient)(rawClient)
              )
            )

            // Scala 2 has no `export`, so the three untouched operations are delegated by hand
            // and only `initialize` adds behaviour on top of the derived implementation.
            new McpServerApi[IO] {
              def ping(): IO[Unit] = impl.ping()

              def listTools(cursor: Option[Cursor], _meta: Option[Map[String, Document]])
                : IO[ListToolsResult] = impl.listTools(cursor, _meta)

              def callTool(
                name: String,
                arguments: Option[Document],
                task: Option[TaskMetadata],
                _meta: Option[Map[String, Document]],
              ): IO[CallToolResult] = impl.callTool(name, arguments, task, _meta)

              def initialize(
                protocolVersion: String,
                capabilities: ClientCapabilities,
                clientInfo: Implementation,
                _meta: Option[Map[String, Document]],
              ): IO[InitializeResult] =
                impl.initialize(
                  protocolVersion,
                  capabilities,
                  clientInfo,
                  _meta,
                ) <* clientCaps.set(Some(capabilities))
            }
          }
          .onFinalize(printErr("Terminating server"))
          .useForever
      }
  }

}
