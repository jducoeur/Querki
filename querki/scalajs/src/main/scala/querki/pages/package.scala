package querki

import querki.globals._

import querki.comm.URL
import querki.data.SpaceInfo

package object pages {
  
  /**
   * The factory for a particular kind of page.
   */
  trait PageFactory {
    /**
     * If this name fits this PageFactory, construct the Page; otherwise, decline and it'll go to the next.
     */
    def constructPageOpt(pageName:String, params:ParamMap):Option[Page]
    
    /**
     * Returns the URL for this Page with these Params.
     */
    def pageUrl(params:(String, String)*):URL
  }
  
  trait Pages extends EcologyInterface {
    
    /**
     * Register a PageFactory for use. Usually called during postInit().
     */
    def registerFactory(factory:PageFactory):PageFactory
    
    /**
     * Convenience wrapper around registerFactory, for the most common case: simply
     * pass in the name of the page and a constructor lambda, and it builds the factory
     * for you.
     */
    def registerStandardFactory(pageName:String, const:ParamMap => Page):PageFactory
    
    /**
     * Given the name and parameters to a Page, build a new instance.
     */
    def constructPage(name:String, params:ParamMap):Page
    
    def exploreFactory:PageFactory
    def viewFactory:PageFactory
    
    /**
     * Navigate to the given Space.
     */
    def showSpacePage(space:SpaceInfo):Unit
    
    /**
     * Display a message on the next Page.
     */
    def flashMessage(error:Boolean, msg:String):Unit
    /**
     * Fetch the message to show, if any. Note that this will reset to None after it gets called.
     */
    def getFlash:Option[(Boolean, String)]
  }
  
  /**
   * Page parameters.
   */
  type ParamMap = Map[String,String]
  implicit class PageParamOps(params:ParamMap) {
    /**
     * Fetch a page parameter that must exist, otherwise it is an error.
     * 
     * This should specifically be used in val's in your Page class; that way, if the param is missing, it
     * will throw an exception during page construction.
     */
    def requiredParam(name:String) = params.get(name).getOrElse(throw new MissingPageParameterException(name))
  }
}
