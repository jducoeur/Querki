package querki.publication

import scala.concurrent.Future

import querki.data._

trait PublicationFunctions {
  def publish(thingId:TID):Future[ThingInfo]
  
  def update(thingId:TID, minor:Boolean):Future[ThingInfo]
}