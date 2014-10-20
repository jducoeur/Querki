package querki.qtext

/**
 * Client Decorator. This varies in that it hashifies all local links.
 */
trait MainDecorator extends Decorator {
  override def decorateLink(text:String, urlIn:String, title:Option[String]):String = {
    val url = if ((urlIn.startsWith("http:") || (urlIn.startsWith("https:") || (urlIn.startsWith("/"))))) {
      // Absolute URL, so leave it alone:
      urlIn
    } else {
      // Relative URL, so hashify it:
      "#" + urlIn
    }
    super.decorateLink(text, url, title)
  }
}
