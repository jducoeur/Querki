package querki

import scala.concurrent.Future

import querki.globals._

import querki.api.StandardThings

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
     * Fetch the specified Thing. If all you need is ThingInfo, go through DataAccess to get it; that way,
     * down the road, we can move towards caching the information client-side.
     */
    def getThing(thingId:TID):Future[ThingInfo]
    
    /**
     * Fetch all of the Properties in this Space. Mainly intended for editing.
     */
    def getAllProps():Future[SpaceProps]
    
    /**
     * Fetch the Types, Collections and Models of this Space. Mainly intended for creating Properties.
     */
    def getAllTypes():Future[AllTypeInfo]
    
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
    
    /**
     * Returns the system-wide StandardThings. Note that this is fetched asynchronously after startup;
     * this Future will resolve once that is ready.
     */
    def standardThings:Future[StandardThings]
    
    /**
     * Returns the system-wide StandardThings. This shortcut assumes that they have been fetched
     * successfully, so only use it after system startup!
     */
    def std = standardThings.value.get.get
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
