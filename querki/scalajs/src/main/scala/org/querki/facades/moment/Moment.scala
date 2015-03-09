package org.querki.facades.moment

import scala.scalajs.js

// TODO: this facade deserves to be fleshed out, properly tested, and published as its
// own open-source library on GitHub.

trait MomentStatic extends js.Object {
  /**
   * Create a Moment from an integer Timestamp.
   * 
   * Note that this can't be a Long, because that isn't a native type.
   */
  def apply(timestamp:Double):Moment = js.native
}

trait Moment extends js.Object {
  /**
   * Return the "calendar time" of this Moment, as a String.
   * 
   * See http://momentjs.com/docs/#/displaying/calendar-time/
   */
  def calendar():String = js.native
}
