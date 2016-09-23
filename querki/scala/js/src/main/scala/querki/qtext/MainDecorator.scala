package querki.qtext

/**
 * Client Decorator. This varies in that it hashifies all local links.
 */
trait MainDecorator extends Decorator {
  override def decorateLink(text:String, urlIn:String, title:Option[String]):String = {
    super.decorateLink(text, MainDecorator.adjustUrl(urlIn), title)
  }
}

object MainDecorator {
  def adjustUrl(urlIn:String):String = {
    // This list of approved protcols should get unified with the one in Decorator:
    if ((urlIn.startsWith("http:") || (urlIn.startsWith("https:") || urlIn.startsWith("mailto:") || (urlIn.startsWith("/")) || (urlIn.startsWith("#"))))) {
      // Absolute URL or already hashed, so leave it alone:
      urlIn
    } else {
      // Relative URL, so hashify it:
      "#!" + urlIn
    }    
  }
}
