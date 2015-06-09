package querki.util

import scala.concurrent.duration._

import akka.actor.{Actor, ReceiveTimeout}
import akka.contrib.pattern.ShardRegion

import querki.globals._

/**
 * Request sent from a child to the parent, telling the parent to kill it.
 */
case object KillMe

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
    val timeout = context.system.settings.config.getDuration(timeoutConfig, java.util.concurrent.TimeUnit.MILLISECONDS)
    context.setReceiveTimeout(Duration(timeout, MILLISECONDS))
    super.preStart()
  }
  
  override def unhandled(message: Any): Unit = {
    message match {
      case resp:ReceiveTimeout => context.parent ! Passivate(KillMe)
      case KillMe => context.stop(self)
      case other => super.unhandled(other)
    }
  }  
}
