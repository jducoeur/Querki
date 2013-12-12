package querki.html

import scala.xml.{Xhtml, XML}

import play.api.templates.Html

import models.{HtmlWikitext}
import models.Thing._
import models.system.{InternalMethod, PropSummary}

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
        PropSummary("Add an class tag to the received HTML value")
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

    }
  }

  override lazy val props = Seq(
    classMethod
  )
}