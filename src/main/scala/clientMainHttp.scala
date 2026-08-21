package app

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import mcptraits.McpServerApi
import modelcontextprotocol.ClientCapabilities
import modelcontextprotocol.Implementation
import my.server.GithubMcpServer
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder

object clientMainHttp extends IOApp {

  def run(args: List[String]): IO[ExitCode] =

    printErr("Starting client") *>
      EmberClientBuilder
        .default[IO]
        .build
        // .map(Logger(logHeaders = true, logBody = true, logAction = Some(IO.println)))
        .evalMap { httpClient =>
          McpBuilder.httpClient(McpServerApi, httpClient, Uri.unsafeFromString(args(0)))
        }
        .onFinalize(printErr("Terminating client"))
        .use { rawServer =>
          implicit val server: McpServerApi[IO] = rawServer

          server
            .initialize(
              protocolVersion = "2025-11-25",
              capabilities = ClientCapabilities(),
              clientInfo = Implementation(name = "smithy4s-mcp-client", "0.0.0"),
            )
            .flatMap { initResult =>
              printErr(
                s"Initialized with: ${initResult.serverInfo.name} ${initResult.serverInfo.version}"
              ) *>
                server.listTools().flatMap { tools =>
                  printErr(s"Available tools: ${tools.tools.map(_.name).mkString(", ")}")
                } *>
                useGithub(McpBuilder.remoteServerStub(GithubMcpServer))
                  .whenA(initResult.serverInfo.name == "github-mcp-server")

            }
        }
        .adaptError { case e => new RuntimeException(s"Failed to run client: ${e.getMessage}", e) }
        .as(ExitCode.Success)

  def useGithub(github: GithubMcpServer[IO]) =
    github.getMe().flatMap { me =>
      printErr(s"Github MCP Server says you are: ${me.login}")
    } *>
      github.listPullRequests("disneystreaming", "smithy4s").flatMap { prs =>
        printErr(s"Smithy4s PRs: ${prs.prs.map(_.title).mkString("\n\n", "\n", "")}")
      }

}
