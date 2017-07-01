package querki.publication

import scala.concurrent.Future

import querki.data._

trait PublicationFunctions {
  def publish(thingId:TID):Future[ThingInfo]
  
  def update(thingId:TID, minor:Boolean):Future[ThingInfo]
  
  /**
   * Call this when you start or stop publishing a Model. It updates the Space accordingly.
   */
  def changePublishedModels():Future[Unit]
  
  /**
   * Drops all unpublished changes to the specified Instance. You should probably get confirmation
   * before this, because it is irreversible!
   */
  def discardChanges(thingId:TID):Future[Unit]
}
