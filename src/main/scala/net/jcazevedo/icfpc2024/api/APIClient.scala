package net.jcazevedo.icfpc2024.api

import java.net.URI

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import org.apache.pekko.actor.{ActorSystem, Terminated}
import org.apache.pekko.http.scaladsl.marshalling.Marshal
import org.apache.pekko.http.scaladsl.model.headers.RawHeader
import org.apache.pekko.http.scaladsl.model.{
  ContentType,
  ContentTypes,
  HttpCharsets,
  HttpMethods,
  HttpRequest,
  MediaType,
  RequestEntity,
  Uri
}
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
import org.apache.pekko.http.scaladsl.{Http, HttpExt}
import org.apache.pekko.util.ByteString

import net.jcazevedo.icfpc2024.config.Config

class APIClient(uri: Uri, headers: List[Config.Header])(implicit system: ActorSystem) {
  private implicit val ec: ExecutionContext =
    system.dispatcher
  private val client: HttpExt =
    Http()

  def post(body: String): Future[String] =
    for {
      requestEntity <- Marshal(body).to[RequestEntity]
      request = HttpRequest(
        method = HttpMethods.POST,
        uri = uri,
        headers = headers.map(header => RawHeader.apply(header.name, header.value)),
        entity = requestEntity.withContentType(ContentType(MediaType.text("icfp"), HttpCharsets.`UTF-8`))
      )
      response <- client.singleRequest(request)
      responseEntity <- Unmarshal(response).to[ByteString]
    } yield responseEntity.utf8String

  def postSync(body: String): String =
    Await.result(post(body), Duration.Inf)
}
