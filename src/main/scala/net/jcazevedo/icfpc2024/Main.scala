package net.jcazevedo.icfpc2024

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

import org.apache.pekko.actor.ActorSystem

import net.jcazevedo.icfpc2024.api.APIClient
import net.jcazevedo.icfpc2024.config.Config
import net.jcazevedo.icfpc2024.lang.Strings

object Main extends App {
  val config = Config.load
  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  val client = new APIClient(config.uri, config.headers)

  def send(message: String): String =
    Strings.fromAlien(client.postSync(s"S${Strings.fromHuman(message)}").tail)

  println(send("get index"))

  Await.ready(system.terminate(), Duration.Inf)
}
