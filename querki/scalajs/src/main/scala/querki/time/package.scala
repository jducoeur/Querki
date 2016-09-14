package querki

import scala.scalajs.js

import org.widok.moment._

import querki.globals._

package object time {
  // HACK to get common time formatting. In the medium term this needs to become much smarter,
  // to respond correctly to localization.
  def displayTime(time:Common.Timestamp):String = {
    Moment(time).calendar(
      js.undefined, 
      lit(
        sameElse = "MM/DD/YYYY"
      ).asInstanceOf[CalendarOpts])
  }
}
