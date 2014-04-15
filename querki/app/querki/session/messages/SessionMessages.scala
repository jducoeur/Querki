package querki.session.messages

import models.ThingId

sealed trait SessionMessage

case class GetThing(thingId:Option[ThingId]) extends SessionMessage

case object GetActiveSessions extends SessionMessage
case class ActiveSessions(n:Int)
