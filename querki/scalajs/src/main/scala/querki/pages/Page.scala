package querki.pages

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._
import querki.display.{Gadget, WrapperDiv}

trait Page extends Gadget[dom.HTMLDivElement] {
  
  /**
   * The title of this page. Concrete subclasses must fill this in.
   */
  def title:String
  
  /**
   * The contents of this page. Concrete subclasses must fill this in.
   */
  def pageContent:Modifier
  
  val contentDiv = new WrapperDiv
  
  def replaceContents(newContent:dom.Element) = {
    contentDiv.replaceContents(newContent)
  }
  
  def doRender() = {
    div(cls:="guts container-fluid",
      div(cls:="row-fluid",
        contentDiv(cls:="querki-content span12",
          pageContent
        )
      )
    )
  }
}
