package querki.test.mid

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc.{Result, Session}

import akka.stream.Materializer
import akka.util.ByteString

/**
 * Synchronous versions of play.api.test.Helpers, for use in non-blocking test code.
 */
class ResultHelpers(result: Result) {
  def headers: Map[String, String] = result.header.headers
  def header(name: String): Option[String] = headers.get(name)
  def sess: Session = result.newSession.getOrElse(Session())
  def status: Int = result.header.status

  def charset: Option[String] =
    result.body.contentType match {
      case Some(s) if s.contains("charset=") => Some(s.split("; *charset=").drop(1).mkString.trim)
      case _                                 => None
    }
  def contentAsBytesFut(implicit mat: Materializer): Future[ByteString] = result.body.consumeData

  def contentAsStringFut(implicit mat: Materializer): Future[String] =
    contentAsBytesFut.map(_.decodeString(charset.getOrElse("utf-8")))
}
