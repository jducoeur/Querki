package querki.display

import scala.concurrent.Future

import org.scalajs.dom.html

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import org.querki.gadgets._
import org.querki.jquery._

import querki.globals._
import querki.pages.ParamMap

/**
 * The description of a single Tab. This isn't quite a proper Gadget -- it is used to build two
 * distinct blocks of HTML -- but you can mostly think of it as one.
 */
abstract class TabGadget(
  val urlName: String,
  val hrefId: String,
  val displayName: String
) {

  /**
   * The concrete Tab must fill this in with the Tab's content. It will typically be a Div.
   */
  def tabContent: Future[Gadget[html.Element]]
}

/**
 * Represents a collection of Tabs. This is not a Gadget per se, in order to allow the Tabs to have
 * their own Futures. Instead, get the Gadget from the tabSetContent.
 */
class TabSetGadget(
  params: ParamMap,
  val tabs: Seq[TabGadget]
)(implicit
  val ecology: Ecology
) extends EcologyMember {

  lazy val PageManager = interface[querki.display.PageManager]

  lazy val initTabName = params.get("tab").getOrElse(tabs.head.urlName)

  class TabLinkGadget(tab: TabGadget) extends Gadget[html.Anchor] {

    // When the user chooses a Tab, change the URL to that Tab, so that reloading goes to the
    // right place:
    override def onCreate(e: html.Anchor) = {
      $ { e }.on(
        "shown.bs.tab",
        { evt: JQueryEventObject =>
          PageManager.modifyPageParams { oldParams =>
            oldParams + ("tab" -> tab.urlName)
          }
        }
      )
    }

    def doRender = a(href := s"#${tab.hrefId}", role := "tab", attr("data-toggle") := "tab", tab.displayName)
  }

  def tabSetContent: Future[Gadget[html.Element]] =
    for {
      contents <- Future.sequence(tabs.map { tab => tab.tabContent.map { content => (tab, content) } })
    } yield div(
      // First we have the tab headings:
      ul(
        cls := "nav nav-tabs",
        role := "tablist",
        tabs.map { tab =>
          li(
            role := "presentation",
            if (initTabName == tab.urlName) cls := "active",
            new TabLinkGadget(tab)
          )
        }
      ),
      // Then the tab contents:
      div(
        cls := "tab-content",
        contents.map { case (tab, content) =>
          section(
            role := "tabpanel",
            if (initTabName == tab.urlName) cls := "tab-pane active" else cls := "tab-pane",
            id := tab.hrefId,
            content
          )
        }
      )
    )
}
