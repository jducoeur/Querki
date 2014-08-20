package querki.util

object SafeUrl {
  def apply(str:String):String = java.net.URLEncoder.encode(str, "UTF-8")
  def decode(str:String):String = java.net.URLDecoder.decode(str, "UTF-8")
}

object HtmlEscape {
  def escapeQuotes(str:String):String = str.replace("\"", "&quot;").replace("'", "&apos;")
}