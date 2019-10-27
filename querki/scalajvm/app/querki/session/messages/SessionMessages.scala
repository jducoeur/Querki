package querki.session.messages

import models._
import querki.values.RequestContext
import autowire.Core

sealed trait SessionMessage extends querki.spaces.messages.SpaceMessagePayload

case object GetActiveSessions extends SessionMessage
case class ActiveSessions(n:Int)

case class ChangeProps2(id:ThingId, changedProps:PropMap) extends SessionMessage

case class MarcoPoloRequest(propId:Option[ThingId], q:String, rc:RequestContext) extends SessionMessage
case class MarcoPoloItem(display:String, id:String)
case class MarcoPoloResponse(items:Seq[MarcoPoloItem])

// Fire-and-forget message to append the given text to the log
case class AddToDebugLog(text: Wikitext) extends SessionMessage
case class GetDebugLog() extends SessionMessage
case class CurrentDebugLog(log: List[Wikitext])
case class ClearDebugLog() extends SessionMessage
case object DebugLogCleared
