package querki

import scala.concurrent.Future

import org.scalajs.dom

import querki.globals._

import querki.comm.URL
import querki.pages.{Page, ParamMap}
import querki.util.Notifier

package object display {
  
  /**
   * The factory function for a Gadget. It is consistent and trivial, but we don't have
   * reflection here, so can't just automate it.
   */
  type GadgetConstr[Output <: dom.Element] = (dom.Element => Gadget[Output])
  type GadgetsConstr[Output <: dom.Element] = (dom.Element => Seq[Gadget[Output]])
  
  trait Gadgets extends EcologyInterface {
    /**
     * Register an InputGadget. Whenever the specified hookClass is encountered, the given Gadget
     * will be wrapped around that Element.
     */
    def registerGadget[Output <: dom.Element](hookClass:String, constr:GadgetConstr[Output]):Unit
    def registerGadgets[Output <: dom.Element](hookClass:String, constr:GadgetsConstr[Output]):Unit

    /**
     * Registers a constructor that can potentially produce multiple Gadgets, or none.
     */
    def registerSimpleGadgets[Output <: dom.Element](hookClass:String, constr: => Seq[Gadget[Output]]):Unit
    
    /**
     * Register an InputGadget that doesn't require fancy construction. This is usually the right
     * answer when the InputGadget doesn't take constructor parameters.
     */
    def registerSimpleGadget[Output <: dom.Element](hookClass:String, constr: => Gadget[Output]):Unit
    
    /**
     * The very simplest form, when you simply want to hook a function that will be run on all elements
     * that match the given selector. This will become a Gadget under the hood, but should usually be
     * used when you don't care about that, and don't plan to create this Gadget in a strongly-typed
     * way on the Client.
     */
    def registerHook(selector:String)(hook:dom.Element => Unit):Unit
    
    /**
     * Given a root element (usually one that has been newly created from server-sent, non-Scalatags code),
     * look for any Gadgets that should be created in it, based on their class.
     * 
     * IMPORTANT: these Gadgets are (intentionally) not immediately hooked! Their hook() method will
     * be called when they actually get added to the DOM and shown, since some controls depend on that.
     */
    def createGadgets(root:dom.Element):Unit    
  }
  
  trait PageManager extends EcologyInterface {
    /**
     * Actually render the page, inside the given root.
     */
    def setRoot(windowIn:dom.Window, root:dom.Element):Unit
    
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
}
