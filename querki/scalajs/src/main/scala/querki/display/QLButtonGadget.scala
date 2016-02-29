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
    val isTextInput:Boolean = (jq.prop("tagName").toOption == Some("INPUT")) && (jq.prop("type").toOption == Some("text"))
    val thingId = jq.tidString("thingid")
    val ql = jq.data("ql").asInstanceOf[String]
    val target = jq.data("target").asInstanceOf[String]
    val append = jq.data("append").map(_.asInstanceOf[Boolean]).getOrElse(false)
    val replace = jq.data("replace").map(_.asInstanceOf[Boolean]).getOrElse(false)
    
    $(elem).addClass("btn-xs")
    
    def activate(evt:JQueryEventObject, actualQL:String) = {
      val targetJQ = $(s"#$target")
      def runQL() = {
        $(elem).addClass("running")
        $(elem).attr("disabled", true)
        Client[ThingFunctions].evaluateQL(thingId, actualQL).call().foreach { result =>
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
        if (append || replace) {
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
    
    if (isTextInput)
      // If it's a text input, we're listening for the Enter key:
      jq.keydown { (evt:JQueryEventObject) =>
        val which = evt.which
        if (which == 13 && jq.valueString.length() > 0) {
          // They pressed Enter
          val input = jq.valueString
          // We need to inject the entered text as the $input binding. We do this simply by
          // tweaking the QL.
          // First, make sure there are no internal ""s, which could allow for code injection:
          val escaped = input.replaceAll("\"\"", "\\\"\"")
          val actualQL = 
            s"""""$escaped"" -> +$$input
$$_context -> $ql"""
          activate(evt, actualQL)
        }
      }
    else
      // Normal button or link -- we're listening for a click:
      jq.click { (evt:JQueryEventObject) =>
        activate(evt, ql)
      }
  }
}
