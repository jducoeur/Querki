package querki.session.messages

import models.ThingId
import models.Thing.PropMap
import querki.values.RequestContext
import autowire.Core

sealed trait SessionMessage

case class GetThing(thingId:Option[ThingId]) extends SessionMessage

case object GetActiveSessions extends SessionMessage
case class ActiveSessions(n:Int)

case class ChangeProps2(id:ThingId, changedProps:PropMap) extends SessionMessage

case class ClientRequest(req:autowire.Core.Request[String], rc:RequestContext)
sealed trait ClientAnswer
case class ClientResponse(pickled:String) extends ClientAnswer
case class ClientError(errorMsg:String) extends ClientAnswer

case class MarcoPoloRequest(propId:Option[ThingId], q:String, rc:RequestContext) extends SessionMessage
case class MarcoPoloItem(display:String, id:String)
case class MarcoPoloResponse(items:Seq[MarcoPoloItem])
