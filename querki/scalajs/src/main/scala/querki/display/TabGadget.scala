package querki.display

import org.scalajs.dom.html

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import org.querki.gadgets._

import querki.globals._
import querki.pages.ParamMap

/**
 * The description of a single Tab. This isn't quite a proper Gadget -- it is used to build two
 * distinct blocks of HTML -- but you can mostly think of it as one.
 */
abstract class TabGadget(val urlName: String, val hrefId: String, val displayName: String) {
  /**
   * The concrete Tab must fill this in with the Tab's content. It will typically be a Div.
   */
  def doRender(): Gadget[html.Element]
}

class TabSetGadget(params:ParamMap, val tabs: Seq[TabGadget]) extends Gadget[html.Div] {
  
  lazy val initTabName = params.get("tab").getOrElse(tabs.head.urlName)
  
  def doRender() =
    div(
      // First we have the tab headings:
      ul(cls:="nav nav-tabs", role:="tablist",
        tabs.map { tab =>
          li(
            role:="presentation", 
            if (initTabName == tab.urlName) cls:="active", 
            a(href:=s"#${tab.hrefId}", role:="tab", attr("data-toggle"):="tab", tab.displayName))          
        }          
      ),
      
      // Then the tab contents:
      div(cls:="tab-content",
        tabs.map { tab =>
          section(
            role := "tabpanel", 
            if (initTabName == tab.urlName) cls := "tab-pane active" else cls := "tab-pane", 
            id := tab.hrefId,
            tab.doRender()
          )
        }
      )
    )
}
