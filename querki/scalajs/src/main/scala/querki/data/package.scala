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
    
    /**
     * The Model of the mainThing.
     */
    def mainModel:Option[ThingInfo]
    
    /**
     * Convenience function to get the "userName" part of a typical path. Should only be used if
     * you know that the space exists!
     */
    def userName = space.get.ownerHandle
    
    /**
     * Convenience function to get the "spaceId" part of a typical path. Should only be used if
     * you know that the space exists!
     */
    def spaceId = space.get.urlName
    
    /**
     * Convenience function to get the "thingId" part of a typical path. Should only be used if
     * you know that the thing exists!
     */
    def thingId = mainThing.get.urlName
  }
  
  /**
   * This trait is used to store the data we fetch from the server.
   */
  trait DataSetting extends EcologyInterface {
    def unpickleRequest(pickled:String)
    def setThing(thing:Option[ThingInfo])
    def setModel(model:Option[ThingInfo])
  }
}
