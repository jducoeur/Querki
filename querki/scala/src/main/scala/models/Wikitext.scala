package models

import language.implicitConversions

import querki.qtext.{MainDecorator, Transformer}
import querki.html.Html
import querki.util.DebugRenderable

case class DisplayText(val str:String) {
  override def toString() = str
  
  def +(other:DisplayText) = new DisplayText(str + other.str)
  // Since a DisplayText is already HTML-neutered, it is safe to encode as HTML:
  def html = Html(str)
  def htmlWikitext = HtmlWikitext(html)
}
object DisplayText {
  implicit def displayText2String(disp:DisplayText) = disp.str
}

sealed trait Wikitext extends DebugRenderable {
  
  def transform(builder: => Transformer)(str:String):String = {
    val transformer = builder
    transformer(str)
  }
  def transformDisplay = transform(new QuerkiTransformer) _
  def transformRaw = transform(new RawTransformer) _
  def transformSpan = transform(new SpanTransformer) _
  
  def display:DisplayText
  
  /**
   * Produces the "raw" string, with minimal markup. Use this for situations where you
   * don't want to allow much Wikitext, such as display names.
   */
  def raw:DisplayText
  
  /**
   * Produces the contents wrapped in a span instead of a div. Intended for cases where you need well-formed XML,
   * but don't want block structure.
   */
  def span:DisplayText
  
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
   * for QText. (Or being used relatively directly from Play, when we know that it will be doing
   * the escaping.) Note that this does no XML escaping!
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
  
  def debugRender = plaintext
}

case class QWikitext(wiki:String) extends Wikitext {
  val keepRaw = false
  
  def display = DisplayText(transformDisplay(internal))
  def raw = DisplayText(transformRaw(internal))
  def span = DisplayText(transformSpan(internal))
  
  /**
   * This should only be used internally, never to display to the user!
   * 
   * We do simple substitutions here, that aren't worth coding into the wikitext engine
   * itself.
   * 
   * Octal 266 is Hex 182, aka the paragraph character. Enter on the numeric keypad as
   * Alt-0182.
   */
  def internal = wiki.replace('\u00b6', '\n')
  
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
 * 
 * Note that the companion object HtmlWikitext intentionally has a different name. This is because upickle.read
 * appears to get confused if the case class has multiple apparent constructors. So construction is
 * always done via the companion, and the actual case class isn't ever used directly.
 */
case class HtmlWikitextImpl(str:String) extends Wikitext {
  def display = DisplayText(str)
  def raw = DisplayText(str)
  def span = DisplayText(str)
  def internal = str
  def plaintext = str
  val keepRaw = true
}
object HtmlWikitext {
  def apply(html:String) = HtmlWikitextImpl(html)
  def apply(html:Html) = HtmlWikitextImpl(html.toString)
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
  def span = DisplayText(process(transformSpan))
  def plaintext = process(str => str)
  def internal = throw new Exception("Nothing should be calling CompositeWikitext.internal!")
  
  val keepRaw = false
}

object Wikitext {
  def apply(str:String):Wikitext = new QWikitext(str)
  val empty = Wikitext("")
  val nl = Wikitext("\n")
}

// NOTE: MainDecorator is defined separately on Client and Server, so they can have slightly
// tweaked behavior.

class QuerkiTransformer extends Transformer with MainDecorator {
    override def deco() = this
    // We now allow XML in QText, but note that the parser only allows a few, whitelisted constructs:
    override def allowVerbatimXml():Boolean = true
    // We use <div> instead of a real <p>, because it turns out that older versions of IE (specifically IE9)
    // do not permit <form>s inside of <p> -- and restructure the HTML to prevent it, breaking our forms.
    override def decorateParagraphOpen():String = """<div class="para">"""
    override def decorateParagraphClose():String = """</div>"""
}

class RawTransformer extends Transformer with MainDecorator {
    override def deco() = this
    override def allowVerbatimXml():Boolean = true
    override def decorateParagraphOpen():String = ""
    override def decorateParagraphClose():String = ""    
}

class SpanTransformer extends Transformer with MainDecorator {
    override def deco() = this
    override def allowVerbatimXml():Boolean = true
    override def decorateParagraphOpen():String = "<span>"
    override def decorateParagraphClose():String = "</span>"    
}
