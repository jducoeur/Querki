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
}
