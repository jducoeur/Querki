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
  
  def transform(builder: => Transformer)(str:String):String = {
    val transformer = builder
    transformer(str)
  }
  def transformDisplay = transform(new QuerkiTransformer) _
  def transformRaw = transform(new RawTransformer) _
  
  def display:DisplayText
  
  /**
   * Produces the "raw" string, with minimal markup. Use this for situations where you
   * don't want to allow much Wikitext, such as display names.
   */
  def raw:DisplayText
  
  /**
   * This should only be used internally, never to display to the user!
   * 
   * We do simple substitutions here, that aren't worth coding into the wikitext engine
   * itself.
   * 
   * Octal 266 is Hex 182, aka the paragraph character. Enter on the numeric keypad as
   * Alt-0182.
   */
  def internal:String
  
  /**
   * Subclasses need to be clear about this. Iff this is set, then we preserve the exact content of
   * this wikitext node, instead of passing it through Wikitexting.
   */
  def keepRaw:Boolean
  
  /**
   * This is the nearly raw, unprocessed text. It should only be used when we are *not* sending
   * to an HTML environment -- generally, when you want to process a text field for QL but not
   * for QText. Note that this does no XML escaping!
   */
  def plaintext:String
  
  /**
   * Used to build composite wikitexts from smaller ones.
   */
  def contents:Seq[Wikitext] = Seq(this)
  
  /**
   * Wikitext can be concatenated just like strings.
   */
  def +(other:Wikitext, insertNewline:Boolean = false):Wikitext = new CompositeWikitext(this, other, insertNewline)
}

case class QWikitext(wiki:String) extends Wikitext {
  val keepRaw = false
  
  def display = DisplayText(transformDisplay(internal))
  def raw = DisplayText(transformRaw(internal))
  
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
}

/**
 * Internal systems can inject HTML into the stream by creating an HtmlWikitext. This will not be
 * processed any further, just inserted directly.
 */
case class HtmlWikitext(html:Html) extends Wikitext {
  private def str = html.toString
  def display = DisplayText(str)
  def raw = DisplayText(str)
  def internal = html.toString
  def plaintext = str
  val keepRaw = true
}

case class CompositeWikitext(left:Wikitext, right:Wikitext, insertNewline:Boolean) extends Wikitext {
  
  /**
   * The flattened contents of all the Wikitexts that built this up. This winds up as a flat sequence of
   * QWikitexts and HtmlWikitexts.
   */
  override def contents:Seq[Wikitext] = {
    (left.contents :+ (if (insertNewline) Wikitext.nl else Wikitext.empty)) ++ right.contents
  }
  
  case class ProcessState(str:String, map:Map[Int, Wikitext])
  
  def process(processor:String => String):String = {
    val indexedContents = contents.zipWithIndex
    // To begin with, we process everything where keepRaw == false, and replace the keepRaw == true...
    val ProcessState(builtStr, substitutionMap) = (ProcessState("", Map.empty[Int, Wikitext]) /: indexedContents) { (state, textAndIndex) =>
      val (text, index) = textAndIndex
      if (text.keepRaw)
        ProcessState(state.str + "(-+" + index + "+-)", state.map + (index -> text))
      else
        ProcessState(state.str + text.internal, state.map)
    }
    val processedStr = processor(builtStr)
    // ... and now we substitute in the keepRaw == true entries:
    val result = (processedStr /: substitutionMap) { (str, entry) =>
      val (index, text) = entry
      str.replaceAllLiterally("(-+" + index + "+-)", text.internal)
    }
    result
  }
  
  def display = DisplayText(process(transformDisplay))
  def raw = DisplayText(process(transformRaw))
  def plaintext = process(str => str)
  def internal = throw new Exception("Nothing should be calling CompositeWikitext.internal!")
  
  val keepRaw = false
}

object Wikitext {
  def apply(str:String):Wikitext = new QWikitext(str)
  val empty = Wikitext("")
  val nl = Wikitext("\n")
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