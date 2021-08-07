package querki.util

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.actor._
import akka.pattern._
import akka.util.Timeout

import querki.globals._

/**
 * Little utility helpers for Akka.
 */
object ActorHelpers {

  def timeout(implicit e: Ecology) = Timeout(Config.getDuration("querki.akka.timeout", 5 seconds))

  implicit class EnhancedActorRef(ref: ActorRef) {

    /**
     * Variant of ask, which will try three times. Use when that seems to be useful, particularly in
     * code that tends to get hit during startup, when things might not be quite fully-baked yet.
     *
     * (Note that this is different from a longer timeout. This is useful for cases where the target
     * Actor doesn't *exist* yet, so the message might just land in deadletters.)
     *
     * Requester has a similar capability built-in, so this should only be used in circumstances where
     * you really mean ask -- that is, *not* from an Actor.
     */
    def askRetry(msg: Any)(implicit timeout: Timeout): Future[Any] = {
      def retrying(n: Int): Future[Any] = {
        if (n == 0)
          Future.failed(new AskTimeoutException(s"Request to send message $msg failed after 3 tries"))
        else {
          (ref ? msg)
            .recoverWith {
              case ex: AskTimeoutException => retrying(n - 1)
            }
        }
      }

      retrying(3)
    }
  }
}
