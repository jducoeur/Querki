package querki.html

import scala.xml._

import play.api.templates.Html

import models.{HtmlWikitext, OID}
import models.Thing._
import models.system.{InternalMethod, PropDetails, PropSummary}

import modules.Module

import ql.QLPhrase

import querki.util._
import querki.values._

class UIModule(val moduleId:Short) extends Module {
  object MOIDs {
    val ClassMethodOID = moid(1)
    val TooltipMethodOID = moid(2)
  }
  import MOIDs._

  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  /**
   * This is the abstraction of a single-parameter function that takes some HTML and modifies it. It is
   * mainly intended for use with functions that change the attributes of the HTML.
   */
  abstract class HtmlModifier(oid:OID, name:String, summary:String, details:String) extends InternalMethod(oid, 
    toProps(
      setName(name),
      PropSummary(summary),
      PropDetails(details))) 
  {
    // Actual Modifier classes should implement this, which does the heart of the work
    def doTransform(elem:Elem, paramText:String):Elem
    
    override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
      val v = context.value
      if (v.pType != RawHtmlType && v.pType != ParsedTextType)
        throw new PublicException("UI.transform.htmlRequired", name)
      if (paramsOpt.isEmpty)
        throw new PublicException("UI.transform.classRequired", name)
      val params = paramsOpt.get

      def processHtml(html:Html):HtmlWikitext = {
        val parsedParamOpt = context.parser.get.processPhrase(params(0).ops, context).value.firstTyped(ParsedTextType)
        if (parsedParamOpt.isEmpty)
          throw new PublicException("UI.transform.classRequired", name)
        val paramText = parsedParamOpt.get.raw.toString
        // TODO: It is truly annoying that I have to handle things this way -- there should be a
        // cleaner way to deal. I begin to suspect that I should be passing around XML instead
        // of raw HTML, so I can manipulate it better.
        val rawHtml = html.body
        val rawXml = XML.loadString(rawHtml)
        val newXml = doTransform(rawXml, paramText)
        val newHtml = Html(Xhtml.toXhtml(newXml))
        HtmlWikitext(newHtml)        
      }
      
      try {
	      v.pType match {
	        case RawHtmlType => {
		      v.map(RawHtmlType, RawHtmlType) { wikitext =>
		        wikitext match {
		          case HtmlWikitext(html) => processHtml(html)
		        }
		      }          
	        }
	        
	        case ParsedTextType => {
		      v.map(ParsedTextType, RawHtmlType) { wikitext => processHtml(wikitext.span.html) }          
	        }
	      }
      } catch {
        case ex:org.xml.sax.SAXParseException => throw new PublicException("UI.transform.notWellFormed", name)
      }
    }
  }
  
  lazy val classMethod = new HtmlModifier(ClassMethodOID, "_class",
      "Add a class tag to the received HTML value",
      """Usually, to add HTML classes to something (to make them look pretty via CSS), you use the
            |\{\{class:...\}\} mechanism. But that *wraps* the text, inside of a div or span, and while that is
            |usually good enough, sometimes it doesn't do everything you need. So _class provides an alternate way
            |to do this via QL -- given an HTML block, this adds the class to *that* block, instead of wrapping it
            |in another.
            |
            |This is mainly intended for use with _edit, to do something like this:
            |[[_code(""[[My Thing -> My Prop._edit -> _class(""myClass"")]]"")]]
            |This will create an Edit input for My Prop, with myClass attached so you can control its display.
            |
            |This can also be used to add a class to a given text block:
            |[[_code(""[[""Hello world"" -> _class(""myClass"")]]"")]]
            |This will create a paragraph for "hello world" as usual, but will attach "myClass" as a class on that
            |paragraph. (This is less often necessary, but occasionally helpful.)""".stripMargin)
  {
    def doTransform(elem:Elem, paramText:String):Elem = HtmlRenderer.addClasses(elem, paramText)
  }
  
  lazy val tooltipMethod = new HtmlModifier(TooltipMethodOID, "_tooltip",
      "Add a tooltip to the received HTML value",
      """When you have a title, or some other short text like that, you sometimes want a "tooltip" -- a little
      |pop-up -- with a better description of what it means. This function lets you add that.
      |
      |Since _tooltip is a function, you have to use it inside a QL expression, like this:
      |[[_code(""[[""My Thing"" -> _tooltip(""My Thing is a special sort of Thing"")]]"")]]
      |In the long run, you will be able to describe a tooltip without using a QL expression, but
      |for now, this is the way to do it.""".stripMargin)
  {
    def doTransform(elem:Elem, paramText:String):Elem = {
      val withClass = HtmlRenderer.addClasses(elem, "_withTooltip")
      withClass % Attribute("title", Text(paramText), Null)
    }
  }

  override lazy val props = Seq(
    classMethod,
    tooltipMethod
  )
}