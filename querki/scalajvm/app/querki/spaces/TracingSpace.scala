package querki.spaces

import querki.ecology.Ecology
import querki.globals.OID
import querki.util.{Config, QLog}

/**
 * Provides deep trace capability for a given Space.
 *
 * To use this, create a TracingSpace val in the object/class in question, and then use its trace()
 * liberally. The trace spews will be turned on by setting `querki.debug.space.[spaceId].trace` to true.
 * This call is cheap unless the flag is turned on, so go wild.
 */
case class TracingSpace(spaceId: OID)(implicit ecology: Ecology) {
  lazy val tracing: Boolean = Config.getBoolean(s"querki.debug.space.${spaceId.toString}.trace", false)

  def trace(msg: => String): Unit = {
    if (tracing) {
      QLog.spew(s"TRACE $spaceId: $msg")
    }
  }
}
