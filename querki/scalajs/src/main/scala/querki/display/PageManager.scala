package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.pages.{Page, ParamMap}

class PageManagerEcot(e:Ecology) extends ClientEcot(e) with PageManager {
  def implements = Set(classOf[PageManager])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val StatusLineInternal = interface[StatusLineInternal]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _displayRoot:Option[dom.Element] = None
  def displayRoot = _displayRoot.get
  
  var _window:Option[dom.Window] = None
  def window = _window.get
  
  override def init() = {
    // Adds the :notUnder() pseudo-selector to jQuery, which selects only elements that are not contained
    // by the specified selector. See: 
    // http://stackoverflow.com/questions/965816/what-jquery-selector-excludes-items-with-a-parent-that-matches-a-given-selector
    // TODO: where does this jQuery hack belong?
    $.expr.asInstanceOf[js.Dynamic].`:`.notUnder = { (elem:dom.Element, i:js.Any, m:js.Array[String]) =>
      $(elem).parents(m(3)).length < 1;
    }
  }
  
  var listeners = Set.empty[PageListener]
  
  def observePageChanges(listener:PageListener) = {
    listeners = listeners + listener
  }
  def unobservePageChanges(listener:PageListener) = {
    listeners = listeners - listener
  }
  
  val menuHolder = new WrapperDiv
  
  def update(title:String) = {
    _window.foreach { w => w.document.title = title }
    menuHolder.replaceContents((new MenuBar).render)
  }
  
  /**
   * Declare the top-level container that we are going to render the page into. This
   * is typically going to be the body itself, but we're deliberately not assuming that.
   */
  @JSExport
  def setRoot(windowIn:dom.Window, root:dom.Element) = {
    _displayRoot = Some(root)
    _window = Some(windowIn)
    
    // The system should all be booted, so let's go render:
    invokeFromHash()

    // Whenever the hash changes, update the window. This is the main mechanism for navigation
    // within the client!
    $(windowIn).on("hashchange", { (evt:JQueryEventObject) =>
      invokeFromHash()
    })
  }
  
  var _imagePath:Option[String] = None
  def imagePath = _imagePath.get
  @JSExport
  def setImagePath(path:String) = {
    _imagePath = Some(path)
  }
  
  var _currentHash = ""
  
  /**
   * Based on the hash part of the current location, load the appropriate page.
   */
  def invokeFromHash() = {
    val fullHash = window.location.hash
    if (fullHash != _currentHash) {
      _currentHash = fullHash
	    // Slice off the hash itself:
	    val hash = fullHash.substring(1)
	    val hashParts = hash.split("\\?")
	    if (hashParts.length == 0)
	      throw new Exception("Somehow wound up with a completely empty hash?")
	    
	    val pageName = hashParts(0)
	    val pageParams =
	      if (hashParts.length == 1)
	        None
	      else
	        Some(hashParts(1).split("&"))
	    val paramMap = pageParams match {
	      case Some(params) => {
	        val pairs = params.map { param =>
	          val pairArray = param.split("=")
	          val key = pairArray(0)
	          val v =
	            if (pairArray.length == 1)
	              "true"
	            else
	              pairArray(1)
	          (key, v)
	        }
	        Map(pairs:_*)
	      }
	      case None => Map.empty[String, String]
	    }
	    
	    renderPage(pageName, paramMap)
    }
  }
  
  def showPage(pageName:String, paramMap:ParamMap) = {
    val paramStr =
      if (paramMap.isEmpty)
        ""
      else
        "?" + paramMap.map(pair => s"${pair._1}=${pair._2}").mkString("&")
    window.location.hash = s"#$pageName$paramStr"
  }
  
  def renderPage(pageName:String, paramMap:ParamMap):Unit = {
    val page = Pages.constructPage(pageName, paramMap)
    renderPage(page)    
  }
  
  /**
   * Actually display the full page.
   */
  def renderPage(page:Page) = {
    val fullPage =
      div(
        StatusLineInternal.statusGadget,
        menuHolder(new MenuBar), 
        page, 
        new StandardFooter)
    
    $(displayRoot).empty()
    $(displayRoot).append(fullPage.render)
    
    listeners.foreach { _(displayRoot, page) }
  }
}
