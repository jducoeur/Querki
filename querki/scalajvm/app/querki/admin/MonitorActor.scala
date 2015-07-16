package querki.admin

import scala.concurrent.duration._

import akka.actor._

import querki.globals._
import Implicits._

/**
 * A simple little Actor that you can hook into a parent in order to send monitor messages,
 * and to heartbeat on a regular basis.
 * 
 * This is broken into a child Actor instead of base class so that it can use the Scheduler 
 * for the heartbeat without interfering with the parent's ReceiveTimeout.
 * 
 * @author jducoeur
 */
class MonitorActor(val ecology:Ecology) extends Actor with EcologyMember {
  
  import MonitorActor._
  
  lazy val AdminOps = interface[AdminOps]
  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  lazy val monitor = AdminOps.monitor
  lazy val scheduler = SystemManagement.actorSystem.scheduler
  
  lazy val heartbeat = Config.getDuration("querki.admin.monitorHeartbeat", 1 minute)
  
  var _current:Option[MonitorEvent] = None
  
  override def preStart() = {
    scheduler.schedule(heartbeat, heartbeat, self, SendUpdate)
    super.preStart()
  }
  
  def sendUpdate() = _current.map(evt => monitor ! evt)
  
  def receive = {
    case msg:MonitorEvent => {
      _current = Some(msg)
      sendUpdate()
    }
    
    case SendUpdate => sendUpdate()
  }
}

object MonitorActor {
  case object SendUpdate
  
  def actorProps(ecology:Ecology) = Props(classOf[MonitorActor], ecology)
}