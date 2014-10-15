package querki.display

import scala.scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import querki.pages.PageIDs.PageID

class PageManagerEcot(e:Ecology) extends ClientEcot(e) with PageManager {
  def implements = Set(classOf[PageManager])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val StatusLineInternal = interface[StatusLineInternal]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _displayRoot:Option[dom.Element] = None
  def displayRoot = _displayRoot.get
  
  override def init() = {
    // Adds the :notUnder() pseudo-selector to jQuery, which selects only elements that are not contained
    // by the specified selector. See: 
    // http://stackoverflow.com/questions/965816/what-jquery-selector-excludes-items-with-a-parent-that-matches-a-given-selector
    // TODO: where does this jQuery hack belong?
    $.expr.asInstanceOf[js.Dynamic].`:`.notUnder = { (elem:dom.Element, i:js.Any, m:js.Array[String]) =>
      $(elem).parents(m(3)).length < 1;
    }
  }
  
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
    val page =
      div(
        StatusLineInternal.statusGadget,
        new MenuBar, 
        Pages.constructPage(pageID, pickled), 
        new StandardFooter)
    
    $(displayRoot).empty()
    $(displayRoot).append(page.render)
  }
}
