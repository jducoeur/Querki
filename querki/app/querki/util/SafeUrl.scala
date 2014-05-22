package querki.util

object SafeUrl {
  def apply(str:String):String = java.net.URLEncoder.encode(str, "UTF-8")
}

object HtmlEscape {
  def escapeQuotes(str:String):String = str.replace("\"", "&quot;").replace("'", "&apos;")
}