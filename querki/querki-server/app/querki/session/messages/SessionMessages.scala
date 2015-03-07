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

// TBD: I do not love the need to pass the RC in the messages, but I'm not seeing an obvious
// way around it -- Thing rendering currently requires it. Think about how to narrow this
// to a smaller and safer data structure.
case class ClientRequest(req:autowire.Core.Request[String], rc:RequestContext) extends SessionMessage
sealed trait ClientAnswer
case class ClientResponse(pickled:String) extends ClientAnswer
case class ClientError(errorMsg:String) extends ClientAnswer

case class MarcoPoloRequest(propId:Option[ThingId], q:String, rc:RequestContext) extends SessionMessage
case class MarcoPoloItem(display:String, id:String)
case class MarcoPoloResponse(items:Seq[MarcoPoloItem])
