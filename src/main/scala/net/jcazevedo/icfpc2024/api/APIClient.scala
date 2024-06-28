package net.jcazevedo.icfpc2024.api

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import com.typesafe.scalalogging.LazyLogging
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshalling.Marshal
import org.apache.pekko.http.scaladsl.model.headers.RawHeader
import org.apache.pekko.http.scaladsl.model.{
  ContentType,
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

class APIClient(uri: Uri, headers: List[Config.Header])(implicit system: ActorSystem) extends LazyLogging {
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
      _ = logger.info(s"Sending message: $body")
      response <- client.singleRequest(request)
      responseEntity <- Unmarshal(response).to[ByteString]
      stringResponse = responseEntity.utf8String
      _ = logger.info(s"Received reply: $stringResponse")
    } yield stringResponse

  def postSync(body: String): String =
    Await.result(post(body), Duration.Inf)
}
