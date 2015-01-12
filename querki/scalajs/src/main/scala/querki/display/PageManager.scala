package querki.display

import scala.concurrent.{Future, Promise}

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.comm.URL
import querki.pages.{MissingPageParameterException, Page, ParamMap}
import querki.util.Notifier

class PageManagerEcot(e:Ecology) extends ClientEcot(e) with PageManager {
  def implements = Set(classOf[PageManager])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val StatusLine = interface[StatusLine]
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
  
  private var _nextChangePromise:Option[Promise[Page]] = None
  def nextChangeFuture:Future[Page] = {
    if (_nextChangePromise.isEmpty)
      _nextChangePromise = Some(Promise[Page])
    _nextChangePromise.get.future
  }
  
  val afterPageLoads = new Notifier[Page] {}
  val beforePageLoads = new Notifier[Page] {}
  
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
    
  def currentHash = _currentHash
  def reload() = invokeFromHash()
  
  /**
   * Based on the hash part of the current location, load the appropriate page.
   */
  def invokeFromHash() = {
    val fullHash = window.location.hash
    
    if (fullHash.length == 0)
      showRoot()
    else
      // Before we "navigate", give any outstanding InputGadgets a chance to save their values:
      InputGadgets.afterAllSaved.foreach { dummy =>
        _currentHash = fullHash
	    // Slice off the hash itself:
	    val hash = fullHash.substring(1)
	    val hashParts = hash.split("\\?")
	    if (hashParts.length == 0)
	      // There is a hash, but nothing else, so it's presumptively root:
	      showRoot()
	    else {
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
	            val key = decode(pairArray(0))
	            val v =
	              if (pairArray.length == 1)
	                "true"
	              else
	                decode(pairArray(1))
	            (key, v)
	          }
	          Map(pairs:_*)
	        }
	        case None => Map.empty[String, String]
	      }
	    
	      renderPage(pageName, paramMap)
	    }
    }
  }
  
  def encode(str:String) = js.encodeURIComponent(str)
  def decode(str:String) = js.decodeURIComponent(str)
  
  def pageUrl(pageName:String, paramMap:ParamMap = Map.empty):URL = {
    val paramStr =
      if (paramMap.isEmpty)
        ""
      else
        "?" + paramMap.map(pair => s"${encode(pair._1)}=${encode(pair._2)}").mkString("&")
    s"#$pageName$paramStr"    
  }
  
  def showPage(pageName:String, paramMap:ParamMap) = {
    window.location.hash = pageUrl(pageName, paramMap)
  }
  
  def showRoot() = {
    // TBD: in principle, this ought to be going through a factory for the Space Thing.
    // But we don't actually *have* a factory for Thing pages. Should we?
    DataAccess.space.foreach(space => showPage(space.urlName.underlying, Map.empty))
  }
  
  def renderPage(pageName:String, paramMap:ParamMap):Unit = {
    try {
      val page = Pages.constructPage(pageName, paramMap)
      renderPage(page)
    } catch {
      case MissingPageParameterException(paramName) => 
        StatusLine.showUntilChange(s"Missing page parameter $paramName")
      case ex:Exception => {
        println(s"Exception trying to render page $pageName")
        ex.printStackTrace()
      }
    }
  }
  
  /**
   * Actually display the full page.
   */
  def renderPage(page:Page) = {
    beforePageLoads(page)
    
    val fullPage =
      div(
        StatusLineInternal.statusGadget,
        menuHolder(new MenuBar), 
        page, 
        new StandardFooter)
    
    page.beforeRender()
        
    $(displayRoot).empty()
    $(displayRoot).append(fullPage.render)
    
    _nextChangePromise.foreach { _.success(page) }
    afterPageLoads(page)
  }
  
  def instantScrollToBottom() = {
    val document = window.document
    $("html, body").scrollTop($(document).jqf.height()-$(window).jqf.height());
  }
}
