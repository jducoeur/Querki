package querki.admin

import akka.actor._

import querki.globals._
import querki.spaces.messages._
import querki.time.DateTime
import querki.util.QuerkiActor

class SpaceTimingActor(e: Ecology) extends QuerkiActor(e) {
  import SpaceTimingActor._

  var msgs = Vector.empty[MonitorMsg]

  def doReceive = {
    case msg: MonitorMsg => msgs = msgs :+ msg

    case SpaceSubsystemRequest(_, _, FetchMsgsSince(i)) => {
      val targetMsgs = msgs.drop(i).map { case MonitorMsg(msg, timestamp) =>
        s"${timestamp.getMillis} -- $msg"
      }
      sender ! NewMsgs(msgs.size, targetMsgs)
    }
  }
}

object SpaceTimingActor {
  def actorProps(e: Ecology) = Props(classOf[SpaceTimingActor], e)

  sealed trait SpaceTimingMsg extends SpaceMessagePayload

  case class MonitorMsg(
    msg: String,
    timestamp: DateTime
  ) extends SpaceTimingMsg
  case class FetchMsgsSince(i: Int) extends SpaceTimingMsg

  case class NewMsgs(
    nowAt: Int,
    msgs: Seq[String]
  )
}
