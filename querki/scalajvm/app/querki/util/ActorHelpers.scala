package querki.util

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

/**
 * Little utility helpers for Akka.
 */
object ActorHelpers {
  
  def timeout = Timeout(Config.getDuration("querki.akka.timeout", 5 seconds))

}
