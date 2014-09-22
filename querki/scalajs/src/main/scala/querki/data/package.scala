package querki

import querki.globals._

package object data {
  /**
   * Provides access to some common data that is used across much of the system.
   */
  trait DataAccess extends EcologyInterface {
    /**
     * The "primary" Thing that the user is currently looking at. The semantics here can get
     * a little dicey, but broadly speaking this is the Thing that determines the current menus.
     */
    def mainThing:Option[ThingInfo]
  }
  
  /**
   * This trait is used only by the API code, and is used to store the data we fetch from the server.
   */
  trait DataSetting extends EcologyInterface {
    def setMainThing(info:ThingInfo)
  }
}
