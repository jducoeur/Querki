package querki.html

import scala.xml.{Xhtml, XML}

import play.api.templates.Html

import models.{HtmlWikitext}
import models.Thing._
import models.system.{InternalMethod, PropDetails, PropSummary}

import modules.Module

import ql.QLPhrase

import querki.util._
import querki.values._

class UIModule(val moduleId:Short) extends Module {
  object MOIDs {
    val ClassMethodOID = moid(1)
  }
  import MOIDs._

  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val classMethod = new InternalMethod(ClassMethodOID,
      toProps(
        setName("_class"),
        PropSummary("Add a class tag to the received HTML value"),
        PropDetails("""Usually, to add HTML classes to something (to make them look pretty via CSS), you use the
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
          ))
  {
    override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
      val v = context.value
      if (v.pType != RawHtmlType && v.pType != ParsedTextType)
        throw new PublicException("UI.class.htmlRequired")
      if (paramsOpt.isEmpty)
        throw new PublicException("UI.class.classRequired")
      val params = paramsOpt.get
      
      def processHtml(html:Html):HtmlWikitext = {
        val classNamesOpt = context.parser.get.processPhrase(params(0).ops, context).value.firstTyped(ParsedTextType)
        if (classNamesOpt.isEmpty)
          throw new PublicException("UI.class.classRequired")
        val classNames = classNamesOpt.get.raw.toString
        // TODO: It is truly annoying that I have to handle things this way -- there should be a
        // cleaner way to deal. I begin to suspect that I should be passing around XML instead
        // of raw HTML, so I can manipulate it better.
        val rawHtml = html.body
        val rawXml = XML.loadString(rawHtml)
        val newXml = HtmlRenderer.addClasses(rawXml, classNames)
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
		      v.map(ParsedTextType, RawHtmlType) { wikitext => processHtml(wikitext.display.html) }          
	        }
	      }
      } catch {
        case ex:org.xml.sax.SAXParseException => throw new PublicException("UI.class.notWellFormed")
      }
    }
  }

  override lazy val props = Seq(
    classMethod
  )
}