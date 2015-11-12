package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.ThingFunctions

class QLButtonGadget[Output <: dom.Element](tag:scalatags.JsDom.TypedTag[Output])(implicit e:Ecology) extends HookedGadget[Output](e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  def doRender() = tag
  
  def hook() = {
    val jq = $(elem)
    val thingId = jq.tidString("thingid")
    val ql = jq.data("ql").asInstanceOf[String]
    val target = jq.data("target").asInstanceOf[String]
    val append = jq.data("append").map(_.asInstanceOf[Boolean]).getOrElse(false)
    
    $(elem).addClass("btn-xs")
    
    jq.click { (evt:JQueryEventObject) =>
      val targetJQ = $(s"#$target")
      def runQL() = {
        $(elem).addClass("running")
        $(elem).attr("disabled", true)
        Client[ThingFunctions].evaluateQL(thingId, ql).call().foreach { result =>
          val qtext = new QText(result)
          if (!append) {
            targetJQ.empty()
          }
          targetJQ.append(qtext.render)
          targetJQ.show()
          $(elem).attr("disabled", false)
          $(elem).removeClass("running")
          $(elem).addClass("open")
          Gadgets.hookPendingGadgets()
        }        
      }
      
      if ($(elem).hasClass("open")) {
        if (append) {
          runQL()
        } else {
          targetJQ.hide()
          $(elem).removeClass("open")
        }
      } else if ($(elem).hasClass("running")) {
        // Query in progress -- don't do anything
      } else {
        runQL()
      }
      
      evt.preventDefault()
    }
  }
}
