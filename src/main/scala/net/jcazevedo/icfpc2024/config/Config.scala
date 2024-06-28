package net.jcazevedo.icfpc2024.config

import org.apache.pekko.http.scaladsl.model.Uri
import pureconfig.generic.semiauto.deriveReader
import pureconfig.module.pekkohttp._
import pureconfig.{ConfigReader, ConfigSource}

case class Config(uri: Uri, headers: List[Config.Header])

object Config {
  case class Header(name: String, value: String)

  object Header {
    implicit val reader: ConfigReader[Header] =
      deriveReader
  }

  implicit val reader: ConfigReader[Config] =
    deriveReader

  def load: Config =
    ConfigSource.default.loadOrThrow[Config]
}
