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
    val thingIdOpt = jq.data("thingid").toOption.map(v => TID(v.asInstanceOf[String]))
    val (typeIdOpt, contextOpt) =
      if (thingIdOpt.isEmpty)
        // Note that the server intentionally prepends a junk char in front of context, to make sure
        // this registers as a "string" in the JavaScript layer. So we drop that:
        (Some(jq.tidString("ptype")), Some(jq.data("context").asInstanceOf[String].drop(1)))
      else
        (None, None)
    val ql = jq.data("ql").asInstanceOf[String]
    val target = jq.data("target").asInstanceOf[String]
    val append = jq.data("append").map(_.asInstanceOf[Boolean]).getOrElse(false)
    val replace = jq.data("replace").map(_.asInstanceOf[Boolean]).getOrElse(false)
    val noIcon = jq.data("noicon").map(_.asInstanceOf[Boolean]).getOrElse(false)
    val (useIcons, openicon, closeicon) =
      if (noIcon || isTextInput || append || replace)
        (false, "", "")
      else
        (true, "glyphicon glyphicon-chevron-down", "glyphicon glyphicon-chevron-up")
    
    if ($(elem).hasClass("btn"))
      $(elem).addClass("btn-xs")
      
    if (useIcons) {
      $(elem).text($(elem).text() + " ")
      $(elem).append(i(cls := "_openaffordance").render)
    }
    
    def setIcon(open:Boolean) = {
      if (useIcons) {
        val afford = $(elem).find("._openaffordance")
        if (open) {          
          afford.removeClass(closeicon)
          afford.addClass(openicon)
        } else {
          afford.removeClass(openicon)
          afford.addClass(closeicon)
        }
      }
    }
    
    setIcon(true)
    
    def activate(evt:JQueryEventObject, actualQL:String) = {
      val targetJQ = $(s"#$target")
      def runQL() = {
        $(elem).addClass("running")
        $(elem).attr("disabled", true)
        
        def handleResult(result:models.Wikitext) = {
          val qtext = new QText(result)
          if (!append) {
            targetJQ.empty()
          }
          targetJQ.append(qtext.render)
          targetJQ.show()
          $(elem).attr("disabled", false)
          $(elem).removeClass("running")
          $(elem).addClass("open")
          setIcon(false)
          Gadgets.hookPendingGadgets()
        }
        
        thingIdOpt match {
          case Some(thingId) => Client[ThingFunctions].evaluateQL(thingId, actualQL).call().foreach(handleResult)
          case None => Client[ThingFunctions].evaluateQLWithContext(typeIdOpt.get, contextOpt.get, actualQL).call().foreach(handleResult)
        }   
      }
      
      if ($(elem).hasClass("open")) {
        if (append || replace) {
          runQL()
        } else {
          targetJQ.hide()
          $(elem).removeClass("open")
          setIcon(true)
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
