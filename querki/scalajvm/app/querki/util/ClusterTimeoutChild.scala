package querki.util

import scala.concurrent.duration._

import akka.actor.{Actor, ReceiveTimeout}
import akka.cluster.sharding.ShardRegion

import querki.globals._

/**
 * Request sent from a child to the parent, telling the parent to kill it.
 */
case object KillMe

case object Shutdown

case object Reload

/**
 * Some standard boilerplate around the timeout-y bits of a ClusterSharding Entity. Entities
 * should usually mix this in -- it allows them to simply set a config flag for their timeout
 * interval, and otherwise not worry about it.
 * 
 * @author jducoeur
 */
trait ClusterTimeoutChild extends Actor {
  import ShardRegion.Passivate
  
  /**
   * Instances must define this -- it is the name of the config string that defines how long
   * the timeout interval is.
   */
  def timeoutConfig:String
  
  /**
   * IMPORTANT: instances must call super.preStart()!!!
   */
  override def preStart() = {
    QLog.spew(s"Starting ClusterTimeoutChild ${self.path}")
    val timeout = context.system.settings.config.getDuration(timeoutConfig, java.util.concurrent.TimeUnit.MILLISECONDS)
    context.setReceiveTimeout(Duration(timeout, MILLISECONDS))
    super.preStart()
  }
  
  abstract override def unhandled(message: Any): Unit = {
    message match {
      case resp:ReceiveTimeout => {
        QLog.spew(s"Passivating (timeout) ClusterTimeoutChild ${self.path}")
        context.parent ! Passivate(KillMe)
      }
      case KillMe => {
        QLog.spew(s"Killing ClusterTimeoutChild ${self.path}")
        context.stop(self)
      }
      case Shutdown => {
        QLog.spew(s"Passivating (Shutdown) ClusterTimeoutChild ${self.path}")
        context.parent ! Passivate(KillMe)
      }
      case Reload => {
        QLog.spew(s"Passivating (Reload) ClusterTimeoutChild ${self.path}")
        context.parent ! Passivate(KillMe)
      }
      case other => super.unhandled(other)
    }
  }  
}
