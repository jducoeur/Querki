package querki.html

// TODO: this is unfortunate abstraction leakage:
import play.twirl.api.Html

/**
 * The Html type. Anything marked as Html is, by definition, safe to render directly
 * with no escaping!
 * 
 * Note that the definition of this type is different between the client and server!
 */
class QHtml(str:String) extends play.twirl.api.Html(str)
object QHtml {
  def apply(str:String):QHtml = QHtml(str)
}
