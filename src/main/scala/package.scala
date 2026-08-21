import cats.effect.IO

/** Scala 2 has no top-level definitions, so shared helpers that used to live at the bottom of
  * smithy4smcp.scala are hosted on the package object instead. Call sites are unchanged.
  */
package object app {

  def printErr(s: String): IO[Unit] = IO.consoleForIO.errorln(s)

  // *> Files[IO]
  //   .writeAll(fs2.io.file.Path("debug.log"))
  //   .apply(fs2.Stream.emit(s + "\n").through(fs2.text.utf8.encode))
  //   .compile
  //   .drain

}
