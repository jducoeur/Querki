package org.querki.facades.bootstrap.datepicker

import scala.scalajs.js
import js.{Date, UndefOr, |}
import js.annotation.JSName
import js.JSConverters._
import org.querki.jsext._
import org.querki.jquery._

@js.native
trait BootstrapDatepicker extends js.Object {
  def datepicker(options: BootstrapDatepickerOptions = ???): JQuery = js.native

  /**
    * Underlying API to send commands to datepicker. Don't use this; use the methods in
    * DatepickerCommands instead.
    */
  @JSName("datepicker")
  def datepickerInternal(cmd: String, params: js.Any*): Any = js.native

  // aliases
  // "-1d"
  val yesterday: String = js.native
  // "+0d"
  val today: String = js.native
  // "+1d"
  val tomorrow: String = js.native
}

/**
  * Strongly-typed versions of the commands defined on BootstrapDatepicker.
  */
class DatepickerCommands(dp: BootstrapDatepicker) {
  def command(cmd: String, params: js.Any*) = dp.datepickerInternal(cmd, params: _*)

  def clearDates() = command("clearDates")

  def getDate() = command("getDate").asInstanceOf[Date]

  def getDates() = command("getDates").asInstanceOf[js.Array[Date]]

  def getFormattedDate() = command("getFormattedDate").asInstanceOf[String]

  def getUTCDate() = command("getUTCDate").asInstanceOf[Date]

  def getUTCDates() = command("getUTCDates").asInstanceOf[js.Array[Date]]

  def hide() = command("hide")

  def remove() = command("remove")

  def setDate(date: Date) = command("setDate", date)

  def setDates(dates: Seq[Date]) = command("setDates", dates.toJSArray)

  def setDaysOfWeekDisabled(v: Boolean) = command("setDaysOfWeekDisabled", v)

  def setDaysOfWeekDisabled(v: Seq[DayOfWeek]) = command("setDaysOfWeekDisabled", v.map(_.underlying).toJSArray)

  def setEndDate(date: Date) = command("setEndDate", date)

  def setStartDate(date: Date) = command("setStartDate", date)

  def setUTCDate(date: Date) = command("setUTCDate", date)

  def setUTCDates(dates: Seq[Date]) = command("setUTCDates", dates.toJSArray)

  def show() = command("show")

  def update(date: String) = command("update", date)

  def update(date: Date) = command("update", date)
}

/**
  * According to the documentation, events defined on BootstrapDatepicker return this enhanced version
  * of JQueryEventObject. See http://bootstrap-datepicker.readthedocs.org/en/latest/events.html
  */
@js.native
trait DatepickerEventObject extends JQueryEventObject {
  def date: Date = js.native

  def dates: js.UndefOr[Array[Date]] = js.native

  def format: js.Function2[UndefOr[Int], UndefOr[String], String] = js.native
}

@js.native
trait BootstrapDatepickerOptions extends js.Object

object BootstrapDatepickerOptions extends BootstrapDatepickerOptionBuilder(noOpts)

/**
  * Options available to BootstrapDatepicker.
  *
  * See http://bootstrap-datepicker.readthedocs.org/en/latest/options.html for full documentation.
  */
class BootstrapDatepickerOptionBuilder(val dict: OptMap) extends JSOptionBuilder[BootstrapDatepickerOptions, BootstrapDatepickerOptionBuilder](new BootstrapDatepickerOptionBuilder(_)) {
  // ******************************
  //
  // Options
  //

  def assumeNearbyYear(v: Boolean | Int) = jsOpt("assumeNearbyYear", v)

  def autoclose(v: Boolean) = jsOpt("autoclose", v)

  def beforeShowDay(v: BeforeShowFunc) = jsOpt("beforeShowDay", v)

  def beforeShowMonth(v: BeforeShowFunc) = jsOpt("beforeShowMonth", v)

  def beforeShowYear(v: BeforeShowFunc) = jsOpt("beforeShowYear", v)

  def beforeShowDecade(v: BeforeShowFunc) = jsOpt("beforeShowDecade", v)

  def beforeShowCentury(v: BeforeShowFunc) = jsOpt("beforeShowCentury", v)

  def calendarWeeks(v: Boolean) = jsOpt("calendarWeeks", v)

  def clearBtn(v: Boolean) = jsOpt("clearBtn", v)

  def container(v: String) = jsOpt("container", v)

  def datesDisabled(v: String) = jsOpt("datesDisabled", v)
  def datesDisabled(v: js.Array[String]) = jsOpt("datesDisabled", v)

  def daysOfWeekDisabled(v: Seq[DayOfWeek]) = jsOpt("daysOfWeekDisabled", v.map(_.underlying).toJSArray)

  def daysOfWeekHighlighted(v: Seq[DayOfWeek]) = jsOpt("daysOfWeekHighlighted", v.map(_.underlying).toJSArray)

  def defaultViewDate(v: Date | String | js.Dictionary[Int]) = jsOpt("defaultViewDate", v)

  def disableTouchKeyboard(v: Boolean) = jsOpt("disableTouchKeyboard", v)

  def enableOnReadonly(v: Boolean) = jsOpt("enableOnReadonly", v)

  def endDate(v: Date | String) = jsOpt("endDate", v)

  def forceParse(v: Boolean) = jsOpt("forceParse", v)

  def format(v: String) = jsOpt("format", v)

  def immediateUpdates(v: Boolean) = jsOpt("immediateUpdates", v)

  def inputs(v: js.Array[JQuery]) = jsOpt("inputs", v)
  
  def keepEmptyValues(v: Boolean) = jsOpt("keepEmptyValues", v)

  def keyboardNavigation(v: Boolean) = jsOpt("keyboardNavigation", v)

  def language(v: String) = jsOpt("language", v)

  def maxViewMode(v: ViewMode) = jsOpt("maxViewMode", v.underlying)

  def minViewMode(v: ViewMode) = jsOpt("minViewMode", v.underlying)

  def multidate(v: Boolean | Int) = jsOpt("multidate", v)

  def multidateSeparator(v: String) = jsOpt("multidateSeparator", v)

  def orientation(v: Orientation*) = jsOpt("orientation", v.map(_.underlying).mkString(" "))

  def showOnFocus(v: Boolean) = jsOpt("showOnFocus", v)
  
  def showWeekDays(v: Boolean) = jsOpt("showWeekDays", v)

  def startDate(v: Date | String) = jsOpt("startDate", v)

  def startView(v: StartView) = jsOpt("startView", v.underlying)

  def templates(v: Object) = jsOpt("templates", v)

  def title(v: String) = jsOpt("title", v)

  def todayBtn(v: Boolean) = jsOpt("todayBtn", v)

  def todayBtnLinked() = jsOpt("todayBtn", "linked")

  def todayHighlight(v: Boolean) = jsOpt("todayHighlight", v)

  def toggleActive(v: Boolean) = jsOpt("toggleActive", v)
  
  def updateViewDate(v: Boolean) = jsOpt("updateViewDate", v)

  def weekStart(v: DayOfWeek) = jsOpt("weekStart", v.underlying)

  def zIndexOffset(v: Int) = jsOpt("zIndexOffset", v)

}

case class DayOfWeek(val underlying: Int) extends AnyVal

object DayOfWeek {
  val Sunday = DayOfWeek(0)
  val Monday = DayOfWeek(1)
  val Tuesday = DayOfWeek(2)
  val Wednesday = DayOfWeek(3)
  val Thursday = DayOfWeek(4)
  val Friday = DayOfWeek(5)
  val Saturday = DayOfWeek(6)
}

case class ViewMode(val underlying: Int) extends AnyVal

object ViewMode {
  val Days = ViewMode(0)
  val Months = ViewMode(1)
  val Years = ViewMode(2)
}

case class StartView(val underlying: Int) extends AnyVal

object StartView {
  val Month = StartView(0)
  val Year = StartView(1)
  val Decade = StartView(2)
}

case class Orientation(val underlying: String) extends AnyVal

object Orientation {
  val Auto = Orientation("auto")
  val Top = Orientation("top")
  val Bottom = Orientation("bottom")
  val Left = Orientation("left")
  val Right = Orientation("right")
}
