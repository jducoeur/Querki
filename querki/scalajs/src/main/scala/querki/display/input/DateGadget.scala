package querki.display.input

import org.scalajs.dom._

import org.querki.jquery._
import org.querki.facades.bootstrap.datepicker._

import querki.globals._

class DateGadget(implicit e:Ecology) extends InputGadget[html.Input](e)  {
  def values = {
    val date = $(elem).getDate()
    val timestamp = date.getTime().toLong
    List(timestamp.toString)
  }
  
  def hook() = {
    $(elem).datepicker(BootstrapDatepickerOptions.
      autoclose(true).
      todayHighlight(true).
      todayBtn(true).
      disableTouchKeyboard(true)
    )
    
    $(elem).on("changeDate", { rawEvt:JQueryEventObject =>
      save()
    })
  }
  
  def doRender() = ???
}
