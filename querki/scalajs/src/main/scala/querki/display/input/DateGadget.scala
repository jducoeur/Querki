package querki.display.input

import org.scalajs.dom._

import org.querki.jquery._
import org.querki.facades.bootstrap.datepicker._

import querki.globals._

class DateGadget(implicit e:Ecology) extends InputGadget[html.Input](e)  {  
  def values = {
    val date = $(elem).getDate()
    if (date == null) {
      List()
    } else {
      val timestamp = date.getTime().toLong
      List(timestamp.toString)
    }
  }
  
  def hook() = {
    val baseOpts = BootstrapDatepickerOptions.
      autoclose(true).
      todayHighlight(true).
      todayBtnLinked().
      disableTouchKeyboard(true).
      orientation(Orientation.Top)
    // Iff this Date is Optional, show the Clear button:
    val opts = 
      if (isOptional)
        baseOpts.clearBtn(true)
      else
        baseOpts
        
    $(elem).datepicker(opts)
    
    $(elem).on("changeDate", { rawEvt:JQueryEventObject =>
      save()
    })
  }
  
  def doRender() = ???
}
