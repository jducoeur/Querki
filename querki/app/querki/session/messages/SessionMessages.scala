package querki.session.messages

import models.ThingId
import models.Thing.PropMap

sealed trait SessionMessage

case class GetThing(thingId:Option[ThingId]) extends SessionMessage

case object GetActiveSessions extends SessionMessage
case class ActiveSessions(n:Int)

case class ChangeProps2(id:ThingId, changedProps:PropMap) extends SessionMessage
