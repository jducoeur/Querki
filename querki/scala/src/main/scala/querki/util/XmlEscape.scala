package querki.util

object XmlEscape {
  
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