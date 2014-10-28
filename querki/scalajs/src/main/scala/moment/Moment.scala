package moment

import scala.scalajs.js

// TODO: this facade deserves to be fleshed out, properly tested, and published as its
// own open-source library on GitHub.

trait MomentStatic extends js.Object {
  /**
   * Create a Moment from an integer Timestamp.
   * 
   * Note that this needs to be declared as a raw Number. Not certain why, but I suspect that Moment
   * is doing some internal type-checking. Passing a Scala Int or Long simply doesn't work.
   */
  def apply(timestamp:js.prim.Number):Moment = ???
}

trait Moment extends js.Object {
  /**
   * Return the "calendar time" of this Moment, as a String.
   * 
   * See http://momentjs.com/docs/#/displaying/calendar-time/
   */
  def calendar():String = ???
}
