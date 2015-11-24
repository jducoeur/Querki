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
    if ((urlIn.startsWith("http:") || (urlIn.startsWith("https:") || (urlIn.startsWith("/")) || (urlIn.startsWith("#"))))) {
      // Absolute URL or already hashed, so leave it alone:
      urlIn
    } else {
      // Relative URL, so hashify it:
      "#!" + urlIn
    }    
  }
}
