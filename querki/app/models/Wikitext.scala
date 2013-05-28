package models

import play.api.Logger
import play.api.templates.Html

import language.implicitConversions

import eu.henkelmann.actuarius.{Decorator, Transformer}

case class DisplayText(val str:String) {
  override def toString() = str
  
  def +(other:DisplayText) = new DisplayText(str + other.str)
}
object DisplayText {
  implicit def displayText2String(disp:DisplayText) = disp.str
}

trait Wikitext {
  def display:DisplayText
  
  /**
   * Produces the "raw" string, with minimal markup. Use this for situations where you
   * don't want to allow much Wikitext, such as display names.
   */
  def raw:DisplayText
  
  /**
   * This is the nearly raw, unprocessed text. It should only be used when we are *not* sending
   * to an HTML environment -- generally, when you want to process a text field for QL but not
   * for QText. Note that this does no XML escaping!
   */
  def plaintext:String
  
  /**
   * Wikitext can be concatenated just like strings.
   */
  def +(other:Wikitext, insertNewline:Boolean = false):Wikitext = new CompositeWikitext(this, other, insertNewline)
}

case class QWikitext(wiki:String) extends Wikitext {
  def display = {
    val transformer = new QuerkiTransformer()
    DisplayText(transformer(internal))
  }
  /**
   * Produces the "raw" string, with minimal markup. Use this for situations where you
   * don't want to allow much Wikitext, such as display names.
   */
  def raw = {
    val transformer = new RawTransformer()
    DisplayText(transformer(internal))
  }
  /**
   * This should only be used internally, never to display to the user!
   * 
   * We do simple substitutions here, that aren't worth coding into the wikitext engine
   * itself.
   * 
   * Octal 266 is Hex 182, aka the paragraph character. Enter on the numeric keypad as
   * Alt-0182.
   */
  def internal = wiki.replace('\266', '\n')
  
  /**
   * This is the nearly raw, unprocessed text. It should only be used when we are *not* sending
   * to an HTML environment -- generally, when you want to process a text field for QL but not
   * for QText. Note that this does no XML escaping!
   */
  def plaintext = internal
  
  /**
   * When we combine two QWikitexts, we want to actually build them into a new object with a
   * combined string, so that the QText parser has everything to work with.
   */
  override def +(other:Wikitext, insertNewline:Boolean) = other match {
    case QWikitext(otherGuts) => QWikitext(internal + (if (insertNewline) "\n" else "") + otherGuts)
    case _ => CompositeWikitext(this, other, insertNewline)
  } 
}

/**
 * Internal systems can inject HTML into the stream by creating an HtmlWikitext. This will not be
 * processed any further, just inserted directly.
 */
case class HtmlWikitext(html:Html) extends Wikitext {
  private def str = html.toString
  def display = DisplayText(str)
  def raw = DisplayText(str)
  def plaintext = str
}

case class CompositeWikitext(left:Wikitext, right:Wikitext, insertNewline:Boolean) extends Wikitext {
  def display = left.display + right.display
  def raw = left.raw + right.raw
  def plaintext = left.plaintext + right.plaintext
  
  // Icky! We need to keep adding QWikitexts together properly after this composite, though.
  // TODO: this whole mechanism is pretty flawed, and needs to be replaced.
  override def +(other:Wikitext, insertNewline:Boolean) = other match {
    case QWikitext(otherGuts) => right match {
      case _:QWikitext => CompositeWikitext(left, right + other, insertNewline)
      case _ => CompositeWikitext(this, other, insertNewline)
    }
    case _ => CompositeWikitext(this, other, insertNewline)
  } 
}

object Wikitext {
  def apply(str:String):Wikitext = new QWikitext(str)
  val empty = Wikitext("")
}

class QuerkiTransformer extends Transformer with Decorator {
    override def deco() = this
    // We no longer allow XML in QText. However, internal systems can inject HTML by using a
    override def allowVerbatimXml():Boolean = false
}

class RawTransformer extends Transformer with Decorator {
    override def deco() = this
    override def allowVerbatimXml():Boolean = false
    override def decorateParagraphOpen():String = ""
    override def decorateParagraphClose():String = ""    
}