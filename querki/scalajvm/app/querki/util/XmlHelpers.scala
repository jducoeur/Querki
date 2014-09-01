package querki.util

import scala.xml._
import scala.xml.parsing.XhtmlParser

object XmlHelpers {
  /**
   * This deals with a pretty common pattern. We're generally passing NodeSeqs around to represent blocks of HTML.
   * *Usually*, this is actually just a single Elem node, but occasionally it's an actual Seq[Elem]. This function
   * figures out which case we have to hand, and applies the given function to that.
   */
  def mapElems(nodes:NodeSeq)(f:Elem => Elem):NodeSeq = {
    nodes match {
      case elem:Elem => f(elem)
      case _ => nodes.flatMap { node => 
        node match {
          case elem:Elem => f(elem)
          case _ => throw new Exception("Got non-Elem element in NodeSeq: " + node)
        }
      }
    }
  }
  
  /**
   * This is a wrapper around XhtmlParser, and deals with its one great weakness: it can't cope with a fragment
   * made up of multiple nodes.
   * 
   * So this copes with our common case: a fragment that is made up of XHTML Elements. Note that anything *outside*
   * the elements will be deliberately ignored, so don't expect miracles here! We explicitly do not preserve Text
   * or CDATAs that fall in here. (We probably could, if we ever decide we care, but that isn't the use case we're
   * worrying about currently.)
   */
  def parseXhtmlFragment(str:String):NodeSeq = {
    val parser = new XhtmlParser(scala.io.Source.fromString(str)).initialize
    
    var nodes = NodeSeq.Empty
    while (!parser.eof) {
      // ... and do the right thing with it:
      if (parser.ch == '<') {
        parser.nextch
        nodes = nodes ++ parser.element1(TopScope)
      } else {
        // Note that, if the current character isn't "<", we just ignore it.
        parser.nextch
      }
    }
    
    nodes
  }
  
  /******************************
   * XML Escaping
   * 
   * This code is lifted directly out of QText's BaseParsers, so that we can do escaping without necessarily
   * going through QText.
   */
  
    /** a mapping of all chars that have to be escaped in xml and the resulting escape sequence
     * The array solution is very ugly, but cuts down block parsing time by 25%
     */
    private val escapedXmlChars = new Array[String](128)
    escapedXmlChars('<') = "&lt;"
    escapedXmlChars('>') = "&gt;"
    escapedXmlChars('"') = "&quot;"
    escapedXmlChars('\'') = "&apos;"
    // TBD: this one screws everything else up. The problem is, if '&' gets escaped naively, then we have
    // no way of properly passing in escaped quotes as &quot; and &apos;.
    // TODO: think this through more carefully. Do we need to escape &? If so, we need to do so with more
    // awareness of XML entities.
//    escapedXmlChars('&') = "&amp;"

    /**
     * Escapes the given char for XML. Returns Either the
     * necessary XML escape Sequence or the same char in a String.
     */
    def escapeForXml(c:Char):String = {
        //looks horrible but massively faster than using a proper map and Option[String]
        val escaped:String = escapeFastForXml(c)
        if (escaped == null) Character.toString(c)
        else                 escaped
    }

    /**
     * Following the same logic as the above, this escapes a String very fast.
     */
    def escapeForXml(str:String):String = {
      val builder = new StringBuilder()
      str.foreach { c =>
        val escaped:String = escapeFastForXml(c)
        if (escaped == null) 
          builder += c
        else
          builder ++= escaped
      }
      builder.toString
    }

    /**
     * Either returns the XML escape sequence for the given char or null.
     * This does not return Option[String] on purpose. While Option[String]
     * would be a much cleaner solution, this is actually called so often
     * that it is a noticeable difference if we use Option here.
     */
    def escapeFastForXml(c:Char) = if (c < escapedXmlChars.length) escapedXmlChars(c)
                                   else null

}