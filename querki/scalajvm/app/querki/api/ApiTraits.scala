package querki.api

import akka.actor.ActorRef

import querki.values.RequestContext

case class ClientRequest(
  req: autowire.Core.Request[String],
  rc: RequestContext
)
sealed trait ClientAnswer
case class ClientResponse(pickled: String) extends ClientAnswer
case class ClientError(errorMsg: String) extends ClientAnswer
case class OversizedResponse(streamer: ActorRef)
