package querki.admin

import scala.concurrent.duration._

import akka.actor._

import org.querki.requester._

import querki.api.{AutowireParams, ClientRequest}
import querki.globals._
import querki.time.DateTime
import querki.values.RequestContext

trait MonitorStats {
  def spaces: Map[OID, SpaceMonitorEvent]
}

/**
 * Second draft of system monitoring.
 *
 * Whereas the (now-deleted) first version of this sent a message to all Spaces
 * and told them to self-report, this one expects to receive regular updates from
 * all of the relevant entities. While Querki is modest-sized, this should work
 * well.
 *
 * In the medium term, we will want to replace this yet again, with a stream-oriented
 * mechanism that is more big-data friendly, probably based on Spark. But one problem
 * at a time.
 *
 * @author jducoeur
 */
class AdminMonitor(implicit val ecology: Ecology) extends Actor with Requester with MonitorStats with EcologyMember {
  import AdminMonitor._

  lazy val ApiInvocation = interface[querki.api.ApiInvocation]
  lazy val SystemManagement = interface[querki.system.SystemManagement]

  lazy val scheduler = SystemManagement.actorSystem.scheduler

  lazy val monitorTimeout = Config.getDuration("querki.admin.monitorTimeout")

  var spaces = Map.empty[OID, SpaceMonitorEvent]
  var watches = Map.empty[ActorPath, MonitorEvent]

  def cleanOldEvents() = {
    val now = DateTime.now
    val old = now.minus(monitorTimeout.toMillis)
    spaces = spaces.filter(_._2.sentTime.isBefore(old))
  }

  def watch(evt: MonitorEvent) = {
    val path = sender.path
    if (watches.get(path).isEmpty) {
      watches += (path -> evt)
      context.watch(sender)
    }
  }

  def mkParams(rc: RequestContext) = AutowireParams(rc.requesterOrAnon, Some(this), rc, this, sender)

  def receive = {
    case evt: SpaceMonitorEvent => { spaces += ((evt.spaceId) -> evt); watch(evt) }

    case ClientRequest(req, rc) => {
      // Note that, in theory, NotificationFunctions is the only thing that'll be routed here:
      ApiInvocation.handleSessionRequest(req, mkParams(rc))
    }

    case Terminated(mon) => {
      watches.get(mon.path) match {
        case Some(evt: SpaceMonitorEvent) => {
          spaces -= evt.spaceId
          watches -= mon.path
        }
        case None => QLog.error(s"AdminMonitor somehow got a Terminated message for unknown Monitor ${mon.path}!")
      }
    }
  }
}

object AdminMonitor {
  def actorProps(ecology: Ecology) = Props(classOf[AdminMonitor], ecology)
}

sealed trait MonitorEvent {
  val sentTime = DateTime.now
}

case class SpaceMonitorEvent(
  spaceId: OID,
  name: String,
  address: String,
  nUsers: Int,
  size: Int
) extends MonitorEvent
