package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.ThingFunctions

class QLButtonGadget[Output <: dom.html.Element](tag:scalatags.JsDom.TypedTag[Output])(implicit e:Ecology) 
  extends HookedGadget[Output](e) with QuerkiUIUtils with EcologyMember 
{
  
  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val Pages = interface[querki.pages.Pages]
  
  def doRender() = tag
  
  def updatePage() = Pages.updatePage(this)
  
  def hook() = {
    val jq = $(elem)
    def tidOpt(name:String) = jq.data(name).toOption.map(v => TID(v.asInstanceOf[String]))
    val isTextInput:Boolean = (jq.prop("tagName").toOption == Some("INPUT")) && (jq.prop("type").toOption == Some("text"))
    val thingIdOpt = tidOpt("thingid")
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
    val noDiv = jq.data("nodiv").map(_.asInstanceOf[Boolean]).getOrElse(false)
    val lexicalOpt = tidOpt("lexical")
    val (useIcons, openicon, closeicon, thinkingicon) =
      if (noIcon || isTextInput || append || replace)
        (false, "", "", "")
      else
        (true, "glyphicon glyphicon-chevron-down", "glyphicon glyphicon-chevron-up", "fa fa-spinner fa-pulse")
    
    if ($(elem).hasClass("btn"))
      $(elem).addClass("btn-sm")
      
    if (useIcons) {
      $(elem).text($(elem).text() + " ")
      $(elem).append(i(cls := "_openaffordance").render)
    }
    
    def setIcon(icon:String) = {
      if (useIcons) {
        val afford = $(elem).find("._openaffordance")
        
        afford.removeClass(openicon)
        afford.removeClass(closeicon)
        afford.removeClass(thinkingicon)
        
        afford.addClass(icon)
      }
    }
    
    setIcon(openicon)
    
    def activate(evt:JQueryEventObject, actualQL:String) = {
      val targetJQ = $(s"#$target")
      def runQL() = {
        $(elem).addClass("running")
        setIcon(thinkingicon)
        $(elem).attr("disabled", true)
        
        def handleResult(result:models.Wikitext) = {
          val qtext:dom.html.Element =
            if (noDiv) {
              // Render this raw, with no ScalaTags wrapper. Do we have a better way to do this?
              $(result.raw.html.toString).get(0).asInstanceOf[dom.html.Element]
            } else {
              (new QText(result)).render
            }
          if (!append) {
            targetJQ.empty()
          }
          targetJQ.append(qtext)
          targetJQ.show()
          $(elem).attr("disabled", false)
          $(elem).removeClass("running")
          $(elem).addClass("open")
          setIcon(closeicon)
          Gadgets.hookPendingGadgets()
          updatePage()
        }
        
        thingIdOpt match {
          case Some(thingId) => Client[ThingFunctions].evaluateQL(thingId, actualQL).call().foreach(handleResult)
          case None => Client[ThingFunctions].evaluateQLWithContext(typeIdOpt.get, contextOpt.get, lexicalOpt, actualQL).call().foreach(handleResult)
        }   
      }
      
      if ($(elem).hasClass("open")) {
        if (append || replace) {
          runQL()
        } else {
          targetJQ.hide()
          $(elem).removeClass("open")
          setIcon(openicon)
          updatePage()
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
