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
case class ClientRequest(apiId:Int, req:autowire.Core.Request[String], rc:RequestContext) extends SessionMessage
case class ClientResponse(pickled:String)
case class ClientError(errorMsg:String)
