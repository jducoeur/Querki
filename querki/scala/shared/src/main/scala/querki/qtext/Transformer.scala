package querki.qtext

import java.io.{InputStreamReader, StringWriter}

/**
 * This is the Transformer that uses the other parsers to transform markdown into xhtml.
 * Mix this trait in if you want more control over the output (like switching verbatim xml on/off or using
 * different opening/closing tags for the output).
 */
trait Transformer { dec: Decorator =>

  private object lineTokenizer extends LineTokenizer(dec) {
    override def allowXmlBlocks() = dec.allowVerbatimXml()
  }

  private object blockParser extends BlockParsers {
    def deco() = dec
  }

  /**
   * This is the method that turns markdown source into xhtml.
   */
  def apply(s: String) = {
    //first, run the input through the line tokenizer
    val lineReader = lineTokenizer.tokenize(s)
    //then, run it through the block parser
    blockParser(lineReader)
  }
}
