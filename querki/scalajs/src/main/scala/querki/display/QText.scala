package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import models.Wikitext

import querki.globals._

import querki.qtext.MainDecorator

trait ServerHtmlHolder extends EcologyMember {
  
  lazy val InputGadgets = interface[input.InputGadgets]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val QTextUtils = interface[querki.qtext.QTextUtils]
  
  def prepContents(root:dom.Element) = {
    val currentHash = PageManager.currentHash
    $(root).find("a").each { (index:js.Any, child:dom.Element) =>
      try {
	    // Adjust the URLs of any links we find.
	    // TODO: this is a serious hack, reflecting the fact that a painful amount of server code
	    // generates <a> tags with raw URLs that don't match current reality. Eventually, we should
	    // fix that somehow -- likely by making the URL scheme consistent everywhere, once we are
	    // more comfortable with the Client.
        val originalHref = $(child).attr("href")
        val fixedHref = QTextUtils.adjustUrl(originalHref)
        $(child).attr("href", fixedHref)
        // HACK, but possibly a necessary one. If this link points to exactly the current hash, it
        // won't do anything. So we need to hook it, to instead become a reload:
        if (fixedHref == currentHash) {
          $(child).click { (evt:JQueryEventObject) =>
            PageManager.reload()
          }
        }
      } catch {
        // Sadly, the JSDom library throws an exception if the tag doesn't have an href
        case e:Exception => { println(s"Got an exception trying to tweak the href of anchor tag $child") }
      }
      1:js.Any
    }
    
    InputGadgets.createInputGadgets(root)    
  }
}

class QText(text:Wikitext, mods:Modifier*)(implicit val ecology:Ecology) extends Gadget[dom.Element] 
  with EcologyMember with ServerHtmlHolder 
{
  override def onCreate(root:dom.Element) = prepContents(root)
  
  def doRender() = {
    // TODO: putting this in a div() is WrongityWrongWrong, since it sometimes might be span-ish.
    // How do we make this appropriately general? Conceptually, a Large Text is a div, and a Text is
    // a span; do we need to distinguish that way somehow?
    div(wikitext(text), mods)
  }
}

/**
 * Minimalist container for inline-structured contents from the server.
 */
class RawSpan(contents:String)(implicit val ecology:Ecology) extends Gadget[dom.Element] 
  with EcologyMember with ServerHtmlHolder 
{
  override def onCreate(root:dom.Element) = prepContents(root)
  
  def doRender() = span(raw(contents))
}

/**
 * Minimalist container for block-structured contents from the server.
 */
class RawDiv(contents:String)(implicit val ecology:Ecology) extends Gadget[dom.Element] 
  with EcologyMember with ServerHtmlHolder 
{
  override def onCreate(root:dom.Element) = prepContents(root)
  
  def doRender() = div(raw(contents))
}
