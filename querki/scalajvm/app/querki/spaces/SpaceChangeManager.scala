package querki.spaces

import scala.concurrent.Future

import akka.actor.Actor

import org.querki.requester._

import models._

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
   * IMPORTANT: this is called by the central update bottleneck for the Space, so it MUST BE FAST. That
   * said, note that TCRReq is a RequestM. Long-lived, complex operations *are* allowed here, but they
   * must map to a new RequestM, and perform the nasty work elsewhere.
   * 
   * Do *NOT* create these intercepts too lightly: they are executed frequently, so if they are too common
   * they will bog down the system.
   */
  val thingChanges = new ThingChangeUpdater

  class ThingChangeUpdater extends Sequencer[TCRReq]
  
  val updateStateCache = new CacheUpdater
  class CacheUpdater extends Sequencer[CacheUpdate]
  
  var spacePluginProviders:Seq[SpacePluginProvider] = Seq.empty
  
  def registerPluginProvider(provider:SpacePluginProvider) = {
    spacePluginProviders = spacePluginProviders :+ provider
  }
}