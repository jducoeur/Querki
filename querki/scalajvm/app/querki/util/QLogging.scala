package querki.util

import play.api.Logger

/**
 * Mix-in trait that provides logging for this class.
 *
 * This is the standard Querki approach to logging (as of Querki3), and should be used freely.
 *
 * Note that this uses the runtime class to determine the identity of the Logger for config purposes. That is
 * *probably* fine, but we'll see if we decide to lift out a more-flexible variant that lets you specify the
 * class.
 */
trait QLogging {

  /**
   * The standard Play Logger.
   *
   * At least for now, we're designing this as a lazy val so as not to always pay the price of initialization on
   * every object. We'll see if that proves wise or not.
   */
  lazy val logger = Logger(this.getClass)
}
