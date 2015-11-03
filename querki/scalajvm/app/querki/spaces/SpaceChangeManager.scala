package querki.spaces

import scala.concurrent.Future

import akka.actor.Actor

import models.{OID, Thing}
import models.Thing.PropMap

import querki.ecology._
import querki.identity.User
import querki.util._
import querki.values._

object SpaceChangeMOIDs extends EcotIds(32)

case class AppLoadInfo(ownerIdentity:OID, spaceId:OID, space:Actor)

/**
 * This is a general mechanism for allowing Modules to listen in on changes before they take effect.
 */
class SpaceChangeManagerEcot(e:Ecology) extends QuerkiEcot(e) with SpaceChangeManager {
  /**
   * Called before every Create or Modify operation. Listeners can use this specifically to edit the Props.
   * 
   * IMPORTANT: this is called by the Space, so it MUST NOT BLOCK.
   */
  val thingChanges = new ThingChangeUpdater

  class ThingChangeUpdater extends Sequencer[ThingChangeRequest]
  
  val updateStateCache = new CacheUpdater
  class CacheUpdater extends Sequencer[CacheUpdate]
  
  /**
   * Called just before Load, to fetch the Apps.
   * 
   * This is a slight abuse of the publication mechanism, but it's the existing way to decouple this
   * functionality from Space itself.
   */
  val appLoader = new AppLoader
  class AppLoader extends Aggregator[AppLoadInfo, Future[Seq[SpaceState]]]
  
  var spacePluginProviders:Seq[SpacePluginProvider] = Seq.empty
  
  def registerPluginProvider(provider:SpacePluginProvider) = {
    spacePluginProviders = spacePluginProviders :+ provider
  }
}