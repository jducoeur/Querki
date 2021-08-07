package querki.util

import akka.actor._
import akka.event.LoggingReceive

import org.querki.requester._

import querki.globals._

/**
 * The standard base class for Querki's Actors. Use this by default: it incorporates all the stuff we're
 * used to having handy, and has some important patterns built-in.
 *
 * @author jducoeur
 */
abstract class QuerkiActor(e: Ecology) extends Actor with Requester with EcologyMember {
  implicit val ecology = e

  /**
   * The standard receive function, which should be implemented by concrete Actors instead
   * of conventional receive.
   *
   * If this Actor has a "boot" mode, use QuerkiBootableActor instead.
   */
  def doReceive: Receive

  protected def mainReceive = LoggingReceive(handleRequestResponse.orElse(doReceive))

  def receive = mainReceive
}

/**
 * Variant of QuerkiActor, for Actors that need to start in a "boot mode" before they can go into
 * their standard processing.
 */
abstract class QuerkiBootableActor(e: Ecology) extends QuerkiActor(e) with Stash {

  /**
   * Concrete Actors should override this for their boot-mode code, and call doneBooting()
   * when finished.
   */
  def bootReceive: Receive

  protected def _bootReceive: Receive = LoggingReceive(handleRequestResponse.orElse(bootReceive).orElse {
    case _ => stash()
  })

  /**
   * Call this from bootReceive when everything's ready to go.
   */
  protected def doneBooting() = {
    unstashAll()
    context.become(mainReceive)
  }

  /**
   * Call this if you need to return to bootReceive.
   */
  protected def reboot() = {
    context.become(_bootReceive)
  }

  override def receive = _bootReceive
}
