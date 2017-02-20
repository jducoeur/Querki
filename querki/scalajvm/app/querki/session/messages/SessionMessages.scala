package querki.session.messages

import models._
import querki.values.RequestContext
import autowire.Core

sealed trait SessionMessage

case object GetActiveSessions extends SessionMessage
case class ActiveSessions(n:Int)

case class ChangeProps2(id:ThingId, changedProps:PropMap) extends SessionMessage

case class MarcoPoloRequest(propId:Option[ThingId], q:String, rc:RequestContext) extends SessionMessage
case class MarcoPoloItem(display:String, id:String)
case class MarcoPoloResponse(items:Seq[MarcoPoloItem])
