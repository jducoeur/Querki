package querki.display

import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom

import querki.globals._

class PageManagerEcot(e:Ecology) extends ClientEcot(e) with PageManager {
  def implements = Set(classOf[PageManager])
  
  @JSExport
  def setRoot(root:dom.Element) = {
    println(s"Going to display in $root")
  }
}
