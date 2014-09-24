package querki.display

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._
import querki.pages.PageIDs.PageID

class PageManagerEcot(e:Ecology) extends ClientEcot(e) with PageManager {
  def implements = Set(classOf[PageManager])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _displayRoot:Option[dom.Element] = None
  def displayRoot = _displayRoot.get
  
  /**
   * Declare the top-level container that we are going to render the page into. This
   * is typically going to be the body itself, but we're deliberately not assuming that.
   */
  @JSExport
  def setRoot(root:dom.Element) = {
    _displayRoot = Some(root)
  }
  
  var _imagePath:Option[String] = None
  def imagePath = _imagePath.get
  @JSExport
  def setImagePath(path:String) = {
    _imagePath = Some(path)
  }
  
  /**
   * Actually display the full page.
   */
  @JSExport
  def renderPage(pageID:PageID, pickled:String) = {
    val menuBar = new MenuBar
    
    val guts = Pages.constructPage(pageID, pickled)
      
    val page =
      div(menuBar, guts)
    
    $(displayRoot).empty()
    $(displayRoot).append(page.render)
  }
}
