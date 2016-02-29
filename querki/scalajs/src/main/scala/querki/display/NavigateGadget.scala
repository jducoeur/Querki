package querki.display

import org.scalajs.dom
import org.querki.jquery._

import querki.globals._

/**
 * This unique little gadget detects "_navigateImmediately" links. These amount to metadata
 * redirects, and cause the Client to immediately navigate to that URL. It comes from the
 * _navigateTo command.
 *  
 * @author jducoeur
 */
class NavigateGadget(implicit e:Ecology) extends HookedGadget[dom.html.Anchor](e) {
  
  lazy val PageManager = interface[PageManager]
  
  def doRender() = ???
  
  def hook() = {
    val jq:JQuery = $(elem)
    val url = jq.attr("href").get
    PageManager.navigateTo(url)
  }
}
