package querki

import querki.globals._

package object data {
  /**
   * Provides access to some common data that is used across much of the system.
   */
  trait DataAccess extends EcologyInterface {
    /**
     * The request that we are responding to.
     */
    def request:RequestInfo
    
    /**
     * The "primary" Thing that the user is currently looking at. The semantics here can get
     * a little dicey, but broadly speaking this is the Thing that determines the current menus.
     */
    def mainThing:Option[ThingInfo]
    
    /**
     * The Space we are currently operating in, if any.
     */
    def space:Option[SpaceInfo]
  }
  
  /**
   * This trait is used only by the API code, and is used to store the data we fetch from the server.
   */
  trait DataSetting extends EcologyInterface {
  }
}
