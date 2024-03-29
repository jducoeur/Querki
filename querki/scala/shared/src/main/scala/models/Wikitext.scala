package models

import language.implicitConversions

import querki.qtext.{MainDecorator, Transformer}
import querki.html.QHtml
import querki.util.DebugRenderable

case class DisplayText(val str: String) {
  override def toString() = str

  def +(other: DisplayText) = new DisplayText(str + other.str)
  // Since a DisplayText is already HTML-neutered, it is safe to encode as HTML:
  def html = QHtml(str)
  def htmlWikitext = HtmlWikitext(html)
}

object DisplayText {
  implicit def displayText2String(disp: DisplayText) = disp.str
}

// TODO: dear lord, can we redo this as a typeclass somehow? The matrix of subclasses of Wikitext and different
// sorts of Transformers is clearly idiotic.
sealed trait Wikitext extends DebugRenderable {

  def transform(builder: => Transformer)(str: String): String = {
    val transformer = builder
    transformer(str)
  }
  def transformDisplay = transform(new QuerkiTransformer) _
  def transformHtml = transform(new HtmlTransformer) _
  def transformRaw = transform(new RawTransformer) _
  def transformSpan = transform(new SpanTransformer) _
  def transformStrip = transform(new StripTransformer) _

  def displayWith(trans: TransformWrapper): DisplayText

  def display: DisplayText

  def html: DisplayText

  /**
   * Produces the "raw" string, with minimal markup. Use this for situations where you
   * don't want to allow much Wikitext, such as display names.
   */
  def raw: DisplayText

  /**
   * Produces the contents wrapped in a span instead of a div. Intended for cases where you need well-formed XML,
   * but don't want block structure.
   */
  def span: DisplayText

  /**
   * Produces a no-markup version, for cases where you really want just plain text output.
   */
  def strip: DisplayText

  /**
   * This should only be used internally, never to display to the user!
   *
   * We do simple substitutions here, that aren't worth coding into the wikitext engine
   * itself.
   *
   * Octal 266 is Hex 182, aka the paragraph character. Enter on the numeric keypad as
   * Alt-0182.
   */
  def internal: String

  /**
   * Subclasses need to be clear about this. Iff this is set, then we preserve the exact content of
   * this wikitext node, instead of passing it through Wikitexting.
   */
  def keepRaw: Boolean

  /**
   * This is the nearly raw, unprocessed text. It should only be used when we are *not* sending
   * to an HTML environment -- generally, when you want to process a text field for QL but not
   * for QText. (Or being used relatively directly from Play, when we know that it will be doing
   * the escaping.) Note that this does no XML escaping!
   */
  def plaintext: String

  def contents: Seq[Wikitext]

  def toComposite(): CompositeWikitext

  /**
   * Wikitext can be concatenated just like strings.
   *
   * Note that this always results in a CompositeWikitext, for ease of management, but will
   * often smash the contents together if they are matching types.
   */
  def +(
    other: Wikitext,
    insertNewline: Boolean = false
  ): Wikitext =
    toComposite().append(other.toComposite(), insertNewline)

  def debugRender = plaintext
}

case class QWikitext(wiki: String) extends Wikitext {
  val keepRaw = false

  def display = DisplayText(transformDisplay(internal))
  def html = DisplayText(transformDisplay(internal))
  def raw = DisplayText(transformRaw(internal))
  def span = DisplayText(transformSpan(internal))
  def strip = DisplayText(transformStrip(internal))

  def displayWith(trans: TransformWrapper) = DisplayText(trans(internal, this))

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

  def plusInternal(
    otherWiki: String,
    insertNewline: Boolean
  ): Wikitext =
    QWikitext(wiki + (if (insertNewline) "\n" else "") + otherWiki)

  def contents = Vector(this)
  def toComposite() = CompositeWikitext(Vector(this))
}

/**
 * Internal systems can inject HTML into the stream by creating an HtmlWikitext. This will not be
 * processed any further, just inserted directly.
 *
 * Note that the companion object HtmlWikitext intentionally has a different name. This is because upickle.read
 * appears to get confused if the case class has multiple apparent constructors. So construction is
 * always done via the companion, and the actual case class isn't ever used directly.
 */
case class HtmlWikitextImpl(str: String) extends Wikitext {
  def display = DisplayText(str)
  def html = DisplayText(str)
  def raw = DisplayText(str)
  def span = DisplayText(str)
  // We don't want to display HTML in strip mode
  // TODO: this is kind of horrible -- we're going to lose the text *in* the HTML as well. When we rewrite this
  // stuff to be ScalaTags-based, we can make it smarter.
  def strip = DisplayText("")

  def displayWith(trans: TransformWrapper) = DisplayText(trans(str, this))

  def internal = str
  def plaintext = str
  val keepRaw = true
  def contents = Vector(this)
  def toComposite() = CompositeWikitext(Vector(this))
}

object HtmlWikitext {
  def apply(html: String) = HtmlWikitextImpl(html)
  def apply(html: QHtml) = HtmlWikitextImpl(html.toString)
}

/**
 * A sequence of Wikitexts that have been appended together.
 *
 * IMPORTANT: there should never be an empty CompositeWikitext!
 *
 * Do not construct this by hand: use the + operator to concatenate Wikitexts.
 */
case class CompositeWikitext(contents: Vector[Wikitext]) extends Wikitext {

  // Check the precondition:
  if (contents.isEmpty)
    throw new Exception("Trying to create an empty CompositeWikitext!")

  /**
   * When appending two CompositeWikitexts together, try to smash the elements that they join at,
   * in order to minimize the number of Wikitext objects we need to transmit to the Client. If
   * the sequences join at a pair of QWikitexts, or a pair of HtmlWikitextImpls, we combine them
   * into a single one.
   *
   * There is probably a more elegant way to do this, but with only two possibilities I'm not
   * worrying about it much.
   */
  private def combine(
    left: Wikitext,
    right: Wikitext,
    insertNewline: Boolean
  ): Vector[Wikitext] = {
    val maybeNewline = if (insertNewline) "\n" else ""
    left match {
      case QWikitext(leftWiki) => right match {
          case QWikitext(rightWiki)  => Vector(QWikitext(leftWiki + maybeNewline + rightWiki))
          case HtmlWikitextImpl(str) => Vector(QWikitext(leftWiki + maybeNewline), right)
          case _                     => throw new Exception("Somehow wound up with nested CompositeWikitexts!")
        }
      case HtmlWikitextImpl(leftStr) => right match {
          case QWikitext(rightWiki)       => Vector(HtmlWikitext(leftStr + maybeNewline), right)
          case HtmlWikitextImpl(rightStr) => Vector(HtmlWikitext(leftStr + maybeNewline + rightStr))
          case _                          => throw new Exception("Somehow wound up with nested CompositeWikitexts!")
        }
      case _ => throw new Exception("Somehow wound up with nested CompositeWikitexts!")
    }
  }

  def append(
    other: CompositeWikitext,
    insertNewline: Boolean
  ): Wikitext = {
    CompositeWikitext(contents.dropRight(1) ++ combine(
      contents.last,
      other.contents.head,
      insertNewline
    ) ++ other.contents.tail)
  }

  case class ProcessState(
    str: String,
    map: Map[Int, Wikitext]
  )

  def process(
    processor: String => String,
    ignoreRaw: Boolean = false
  ): String = {
    val indexedContents = contents.zipWithIndex
    // To begin with, we process everything where keepRaw == false, and replace the keepRaw == true...
    val ProcessState(builtStr, substitutionMap) =
      (ProcessState("", Map.empty[Int, Wikitext]) /: indexedContents) { (state, textAndIndex) =>
        val (text, index) = textAndIndex
        if (text.keepRaw) {
          if (ignoreRaw) {
            // Simply skip this element. We do this during strip mode:
            state
          } else {
            ProcessState(state.str + "(-+" + index + "+-)", state.map + (index -> text))
          }
        } else
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
  def html = DisplayText(process(transformHtml))
  def raw = DisplayText(process(transformRaw))
  def span = DisplayText(process(transformSpan))
  def strip = DisplayText(process(transformStrip, true))
  def displayWith(trans: TransformWrapper) = DisplayText(process({ str => trans(str, this) }, trans.ignoreRaw))
  def plaintext = process(str => str)
  def internal = throw new Exception("Nothing should be calling CompositeWikitext.internal!")

  val keepRaw = false

  def toComposite() = this
}

object Wikitext {
  def apply(str: String): Wikitext = new QWikitext(str)
  val empty = Wikitext("")
  val nl = Wikitext("\n")
}

trait TransformWrapper {
  def transformer: Transformer

  val ignoreRaw: Boolean = false

  def apply(
    str: String,
    wiki: Wikitext
  ) = transformer(str)
}

// NOTE: MainDecorator is defined separately on Client and Server, so they can have slightly
// tweaked behavior.

class QuerkiTransformer extends Transformer with MainDecorator {
  // We now allow XML in QText, but note that the parser only allows a few, whitelisted constructs:
  override def allowVerbatimXml(): Boolean = true
  // We use <div> instead of a real <p>, because it turns out that older versions of IE (specifically IE9)
  // do not permit <form>s inside of <p> -- and restructure the HTML to prevent it, breaking our forms.
  override def decorateParagraphOpen(): String = """<div class="para">"""
  override def decorateParagraphClose(): String = """</div>"""
}

class HtmlTransformer extends Transformer with MainDecorator {
  override def allowVerbatimXml(): Boolean = true
}

class RawTransformer extends Transformer with MainDecorator {
  override def allowVerbatimXml(): Boolean = true
  override def decorateParagraphOpen(): String = ""
  override def decorateParagraphClose(): String = ""
}

class SpanTransformer extends Transformer with MainDecorator {
  override def allowVerbatimXml(): Boolean = true
  override def decorateParagraphOpen(): String = "<span>"
  override def decorateParagraphClose(): String = "</span>"
}

class StripTransformer extends Transformer with MainDecorator {
  override def allowVerbatimXml(): Boolean = true

  override def decorateBreak(): String = "\n"
  override def decorateCode(code: String): String = "`" + code + "`"
  override def decorateEmphasis(text: String): String = "*" + text + "*"
  override def decorateStrong(text: String): String = "**" + text + "**"
  override def decorateStrike(text: String): String = "-" + text + "-"

  override def decorateLink(
    text: String,
    url: String,
    title: Option[String]
  ): String = text

  override def decorateImg(
    alt: String,
    src: String,
    title: Option[String]
  ): String = ""
  override def decorateRuler(): String = "======\n"
  override def decorateHeaderOpen(headerNo: Int): String = ""
  override def decorateHeaderClose(headerNo: Int): String = ""
  override def decorateCodeBlockOpen(): String = "\n\n"
  override def decorateCodeBlockClose(): String = "\n\n"
  override def decorateParagraphOpen(): String = ""
  override def decorateParagraphClose(): String = ""
  override def decorateBlockQuoteOpen(): String = "\n"
  override def decorateBlockQuoteClose(): String = "\n"
  override def decorateItemOpen(): String = "* "
  override def decorateItemClose(): String = "\n"
  override def decorateUListOpen(): String = ""
  override def decorateUListClose(): String = "\n"
  override def decorateOListOpen(): String = ""
  override def decorateOListClose(): String = "\n"
  override def decorateDListOpen(): String = ""
  override def decorateDListClose(): String = "\n"
  override def decorateDTitleOpen(): String = ""
  override def decorateDTitleClose(): String = "\n"
  override def decorateDDescOpen(): String = "    "
  override def decorateDDescClose(): String = "\n"
  override def decorateClassDivOpen(className: String): String = ""
  override def decorateClassDivClose(): String = ""

  override def decorateClassSpan(
    className: String,
    text: String
  ): String = text
}

class LiteralTransformer(showParas: Boolean) extends StripTransformer {
  override def decorateParagraphOpen(): String = ""
  override def decorateParagraphClose(): String = if (showParas) "\n\n" else ""
  override def escapeXmlEntities(): Boolean = false
}

class LiteralTransformWrapper(showParas: Boolean = true) extends TransformWrapper {
  val transformer = new LiteralTransformer(showParas)
  override val ignoreRaw: Boolean = true
}
