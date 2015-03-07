package querki.qtext

/**
 * This trait influences the behavior of the Markdown output of inline and block parsers
 * and the complete transformer.
 * Mix in this trait and override methods to change the behavior and override the "deco()" method
 * in the respective parser/transformer to return
 * your modified instances to change the output they create.
 *
 * Inline element decoration methods always get passed the spanned text, so you have to
 * prepend and append the opening/closing tags. For block elements there is always a method
 * for the opening and closing tags. This is to make block
 * processing more efficient to prevent unnecessary String building of whole blocks just to
 * add tags. (The block building uses a StringBuilder internally and just appends the returned tags)
 *
 * If you want line breaks after opening/closing block level tags, you have to add the newline yourself.
 */

trait Decorator {
    /**
     * The string used to ident one level. Defaults to the empty string
     */
    def indentation() = ""
    /**
     * If true, inline xml tags and verbatim xml blocks are allowed,
     * otherwise they are escaped and included as plain text
     */
    def allowVerbatimXml():Boolean = true
    /** used to print out manual line breaks (default: <br />)
     */
    def decorateBreak():String = "<br />"
    /** used to print out inline code (default: <code>...</code>)
     */
    def decorateCode(code:String):String = "<code>" + code + "</code>"
    /** used to print out emphasized text (default <em>...</em>)
     */
    def decorateEmphasis(text:String):String = "<em>" + text + "</em>"
    /** Used to print out strong text (default: <strong>...</strong>
     */
    def decorateStrong(text:String):String = "<strong>" + text + "</strong>"
    /** Used to print link elements (default: <a href...)
     */
    def javascriptNeutralized(url:String):String = {
      // Basically, if this has anything that looks like an unapproved URI scheme, force it to relative:
      val urlLower = url.toLowerCase()
      if (!(urlLower.startsWith("http:") || urlLower.startsWith("https:")) && urlLower.matches("^[a-z0-9+\\.\\-]*:.*"))
        "./" + url
      else
        url  
    }
    def decorateLink(text:String, url:String, title:Option[String]):String = title match {
        case None    => "<a href=\"" + javascriptNeutralized(url) + "\" rel=\"nofollow\">" + text + "</a>"
        case Some(t) => "<a href=\"" + javascriptNeutralized(url) + "\" title=\"" + t + "\" rel=\"nofollow\">" + text + "</a>"
    }
    /** Used to print image elements (default: <img ...)
     */
    def decorateImg(alt:String, src:String, title:Option[String]):String = title match {
        case None    => "<img src=\"" + javascriptNeutralized(src) + "\" alt=\"" + alt + "\" />"
        case Some(t) => "<img src=\"" + javascriptNeutralized(src) + "\" alt=\"" + alt + "\" title=\"" + t + "\" />"
    }
    /**used to print a horizontal ruler defaults to "<hr />\n" */
    def decorateRuler():String = "<hr />\n"
    /** used to print the beginning of a header, defaults to "<h[headerNo]>" */
    def decorateHeaderOpen(headerNo:Int):String = "<h" + headerNo + ">"
    /** used to print the end of a header, defaults to "</h[headerNo]\n>" */
    def decorateHeaderClose(headerNo:Int):String = "</h" + headerNo + ">\n"
    /** used to print the beginning of a code block, defaults to "<pre><code>"*/
    def decorateCodeBlockOpen():String = "<pre><code>"
    /** used to print the end of a code block, defaults to "</code></pre>\n" */
    def decorateCodeBlockClose():String = "</code></pre>\n"
    /** used to print the beginning of a paragraph, defaults to "<p>" */
    def decorateParagraphOpen():String = "<p>"
    /** used to print the end of a paragraph, defaults to "</p>\n" */
    def decorateParagraphClose():String = "</p>\n"
    /** used to print the beginning of a blockquote, defaults to "<blockquote>" */
    def decorateBlockQuoteOpen():String = "<blockquote>"
    /** used to print the end of a blockquote, defaults to "</blockquote>\n" */
    def decorateBlockQuoteClose():String = "</blockquote>\n"
    /** used to print the beginning of a list item, defaults to "<li>" */
    def decorateItemOpen():String = "<li>"
    /** used to print the end of a list item, defaults to "</li>" */
    def decorateItemClose():String = "</li>\n"
    /** used to print the beginning of an unordered list, defaults to "<ul>\n" */
    def decorateUListOpen():String = "<ul>\n"
    /** used to print the end of an unordered list, defaults to "</ul>\n" */
    def decorateUListClose():String = "</ul>\n"
    /** used to print the beginning of an ordered list, defaults to <ol>\n */
    def decorateOListOpen():String = "<ol>\n"
    /** used to print the end of an ordered list, defaults to </ol>\n */
    def decorateOListClose():String = "</ol>\n"
      
    /** used to print the beginning of a definition list, defaults to "<dl>\n" */
    def decorateDListOpen():String = "<dl>\n"
    /** used to print the end of a definition list, defaults to "</dl>\n" */
    def decorateDListClose():String = "</dl>\n"
    /** used to print the beginning of a definition, defaults to "<li>" */
    def decorateDTitleOpen():String = "<dt>"
    /** used to print the end of a definition, defaults to "</li>" */
    def decorateDTitleClose():String = "</dt>\n"
    /** used to print the beginning of a definition, defaults to "<li>" */
    def decorateDDescOpen():String = "<dd>"
    /** used to print the end of a definition, defaults to "</li>" */
    def decorateDDescClose():String = "</dd>\n"
      
    /** decorate the beginning and end of a class-defining div for style declaration */
    def decorateClassDivOpen(className:String):String = "<div class=\"" + className + "\">\n"
    def decorateClassDivClose():String = "</div>\n"
    def decorateClassSpan(className:String, text:String):String = "<span class=\"" + className + "\">" + text + "</span>" 
}

/**
 * Default instance of Decorator with the standard Markdown behavior
 */
object Decorator extends Decorator {
}