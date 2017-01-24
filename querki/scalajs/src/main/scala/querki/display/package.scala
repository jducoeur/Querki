package querki

import scala.concurrent.Future

import org.scalajs.dom
import dom.html.Element

import querki.globals._

import querki.api.OperationHandle
import querki.comm.URL
import querki.pages.{Page, ParamMap}
import querki.util.Notifier
import org.querki.gadgets.core.ManagedFrag

package object display {
  
  /**
   * The factory function for a Gadget. It is consistent and trivial, but we don't have
   * reflection here, so can't just automate it.
   */
  type GadgetConstr[Output <: Element] = (Element => Gadget[Output])
  type GadgetsConstr[Output <: Element] = (Element => Seq[Gadget[Output]])
  
  trait Gadgets extends EcologyInterface {
    /**
     * Register an InputGadget. Whenever the specified hookClass is encountered, the given Gadget
     * will be wrapped around that Element.
     */
    def registerGadget[Output <: Element](hookClass:String, constr:GadgetConstr[Output]):Unit
    def registerGadgets[Output <: Element](hookClass:String, constr:GadgetsConstr[Output]):Unit

    /**
     * Registers a constructor that can potentially produce multiple Gadgets, or none.
     */
    def registerSimpleGadgets[Output <: Element](hookClass:String, constr: => Seq[Gadget[Output]]):Unit
    
    /**
     * Register an InputGadget that doesn't require fancy construction. This is usually the right
     * answer when the InputGadget doesn't take constructor parameters.
     */
    def registerSimpleGadget[Output <: Element](hookClass:String, constr: => Gadget[Output]):Unit
    
    /**
     * The very simplest form, when you simply want to hook a function that will be run on all elements
     * that match the given selector. This will become a Gadget under the hood, but should usually be
     * used when you don't care about that, and don't plan to create this Gadget in a strongly-typed
     * way on the Client.
     */
    def registerHook(selector:String)(hook:Element => Unit):Unit
    
    /**
     * Given a root element (usually one that has been newly created from server-sent, non-Scalatags code),
     * look for any Gadgets that should be created in it, based on their class.
     * 
     * IMPORTANT: these Gadgets are (intentionally) not immediately hooked! Their hook() method will
     * be called when they actually get added to the DOM and shown, since some controls depend on that.
     */
    def createGadgets(root:Element):Unit    
    
    /**
     * Hook all Gadgets that have been created but not yet hooked.
     * 
     * IMPORTANT: you *MUST* call this any time you add Page content that may potentially contain
     * HookedGadgets! But this must be called *AFTER* that content is fully added to the DOM tree,
     * and shown!
     */
    def hookPendingGadgets():Unit
  }
  
  trait PageManager extends EcologyInterface {
    /**
     * Actually render the page, inside the given root.
     */
    def setRoot(windowIn:dom.Window, root:dom.Element):Future[Page]
    
    /**
     * The window that we are operating within.
     */
    def window:dom.Window
    
    /**
     * Set the folder where images are kept.
     */
    def setImagePath(path:String):Unit
    
    /**
     * The URL path to get to the system images.
     */
    def imagePath:String
    
    /**
     * Update the current Page's display. This is called after the Page fetches its contents.
     */
    def update(title:String):Unit
    
    /**
     * Returns the URL for the specified Page.
     */
    def pageUrl(pageName:String, paramMap:ParamMap = Map.empty):URL
    
    /**
     * Switch to the specified page. This is fairly low-level; use higher-level APIs when possible.
     */
    def showPage(pageName:String, paramMap:ParamMap):Future[Page]
    
    /**
     * Show the root of the current Space.
     */
    def showRoot():Future[Page]
    
    /**
     * If you need to be signaled when the page next changes, use this.
     * 
     * IMPORTANT: this is an edge-trigger! Use with care! After the page changes, this will reset
     * to a new Future.
     */
    def nextChangeFuture:Future[Page]
    
    /**
     * The current local part of the URL -- essentially the relative URL.
     */
    def currentHash:String
    
    /**
     * Reload the current page, based on its hash.
     */
    def reload():Future[Page]
    
    /**
     * Completely reloads the Client, with the current URL. This should only be used in
     * relatively extreme situations, such as when we know the Client is out of date against
     * the Server.
     */
    def fullReload():Unit
    
    /**
     * Listen to this publication point if you want to be notified *before* each Page load.
     */
    def beforePageLoads:Notifier[Page]
    
    /**
     * Listen to this publication point if you want to be notified *after* each Page load.
     */
    def afterPageLoads:Notifier[Page]
    
    /**
     * Scrolls to the bottom of the page.
     */
    def instantScrollToBottom():Unit
    
    /**
     * Each page calls this when it is *finished* rendering; this in turn kicks off the
     * afterPageLoads and nextChangeFuture events.
     */
    def onPageRendered(page:Page):Unit
    
    /**
     * Navigate to the main Querki index page.
     * 
     * IMPORTANT: this will currently cause this client to be unloaded!
     */
    def showIndexPage():Unit
    
    /**
     * Navigates to the given URL.
     * 
     * IMPORTANT: this will currently cause this client to be unloaded!
     */
    def navigateTo(url:String):Unit
    
    /**
     * The parameters to the current page.
     * 
     * We need to expose these so that they can be passed as metadata to the server.
     */
    def currentPageParams:ParamMap
    
    /**
     * The internal render mechanism. You should *not* normally use this! It is exposed solely so that workflows
     * can build and use Pages directly.
     */
    def renderPage(page:Page):Future[Page]
  }
  
  trait StatusLine extends EcologyInterface {
    /**
     * Display the given message for a few seconds.
     */
    def showBriefly(msg:String):Unit
    
    /**
     * Display the given message until there is another show.
     */
    def showUntilChange(msg:String):Unit
    
    /**
     * Hide the status line.
     */
    def clear():Unit
  }
  
  trait ProgressDialog extends EcologyInterface {
    def showDialog(processName:String, operation:OperationHandle, onSuccess: => Unit, onFailure: => Unit):Unit
  }
}
