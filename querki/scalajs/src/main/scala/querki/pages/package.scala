package querki

import querki.globals._

package object pages {
  trait Pages extends EcologyInterface {
    /**
     * Given the name and parameters to a Page, build a new instance.
     */
    def constructPage(name:String, params:ParamMap):Page
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
