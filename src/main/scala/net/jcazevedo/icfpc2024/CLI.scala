package net.jcazevedo.icfpc2024

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

import org.apache.pekko.actor.ActorSystem
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.{Terminal, TerminalBuilder}

import net.jcazevedo.icfpc2024.api.APIClient
import net.jcazevedo.icfpc2024.config.Config
import net.jcazevedo.icfpc2024.lang.Strings

object CLI extends App {
  val config: Config =
    Config.load

  val terminal: Terminal =
    TerminalBuilder.terminal()

  val reader: LineReader =
    LineReaderBuilder.builder().terminal(terminal).build()

  implicit val system: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = system.dispatcher
  val client = new APIClient(config.uri, config.headers)

  def processLine(line: String): Unit = {
    val result = Strings.fromAlien(client.postSync(s"S${Strings.fromHuman(line)}").tail)
    println(result)
  }

  def loop(): Unit = {
    var more: Boolean = true

    while (more) {
      val line: String =
        try {
          reader.readLine("icfpc2024> ")
        } catch {
          case _: UserInterruptException =>
            println("Press Control-D to exit")
            ""

          case _: EndOfFileException => return
        }

      if (line != null) {
        val trimmed = line.trim

        if (trimmed.equalsIgnoreCase("quit") || trimmed.equalsIgnoreCase("quit"))
          more = false
        else
          processLine(line)
      }
    }
  }

  loop()
  Await.ready(system.terminate(), Duration.Inf)
}
