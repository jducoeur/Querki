package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.display.input.InputGadget

class QLButtonGadget[Output <: dom.Element](tag:scalatags.JsDom.TypedTag[Output])(implicit e:Ecology) extends InputGadget[Output](e) with EcologyMember {
  
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  
  def doRender() = tag
  def values = ???
  
  def hook() = {
    val jq = $(elem)
    val thingId = jq.tidString("thingid")
    val ql = jq.data("ql").asInstanceOf[String]
    val target = jq.data("target").asInstanceOf[String]
    
    $(elem).addClass("btn-xs")
    
    jq.click { (evt:JQueryEventObject) =>
      val targetJQ = $(s"#$target")
      if ($(elem).hasClass("open")) {
        targetJQ.hide()
        $(elem).removeClass("open")
      } else if ($(elem).hasClass("running")) {
        // Query in progress -- don't do anything
      } else {
        $(elem).addClass("running")
        $(elem).attr("disabled", true)
        Client[ThingFunctions].evaluateQL(thingId, ql).call().foreach { result =>
          val qtext = new QText(result)
          targetJQ.empty()
          targetJQ.append(qtext.render)
          targetJQ.show()
          $(elem).attr("disabled", false)
          $(elem).removeClass("running")
          $(elem).addClass("open")
          InputGadgets.hookPendingGadgets()
        }
      }
      
      evt.preventDefault()
    }
  }
}
