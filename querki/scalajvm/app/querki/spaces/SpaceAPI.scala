package querki.spaces

import akka.actor._

import org.querki.requester._

import models.{OID, Thing, ThingId}
import models.Thing.PropMap

import querki.identity.User
import querki.values.SpaceState

/**
 * The abstraction of a Space, so that SpacePlugins can interact with it.
 * 
 * IMPORTANT: this must NEVER be used outside of a SpacePlugin, when that Plugin has been properly
 * invoked by the Space itself! Anything else is incredibly dangerous!
 */
trait SpaceAPI extends Actor with Requester {
  /**
   * Accessor to the current state of this Space. Note that you cannot modify the state directly --
   * you must do so through modifyThing.
   */
  def state:SpaceState
  
  /**
   * Record a change to the specified Thing.
   * 
   * IMPORTANT: this will send responses to the current Akka sender, but may do so asynchronously. 
   * It will also persist the changes, asynchronously.
   * 
   * @param who The User who is requesting this Change.
   * @param thingId The ThingId of the Thing to be modified.
   * @param modelIdOpt ''If'' the Thing's Model is being changed, the OID of the new Model. Otherwise,
   *    this can be omitted.
   * @param pf A Function that takes the previous state of the Thing, and returns the *complete* new
   *    PropMap for that Thing. (Not just the changes!)
   * @param sync If true, this change will be made synchronously -- sender will not be informed of
   *    the change until after it is persisted. This is mainly intended for cases where there is a real
   *    chance that the Space Actor will go away before the change gets flushed, but this might become
   *    the default.
   */
  def modifyThing(who:User, thingId:ThingId, modelIdOpt:Option[OID], pf:(Thing => PropMap), sync:Boolean = false):Unit
}

/**
 * A SpacePlugin allows external Ecots to add specialized messages that each Space can handle.
 * This is mainly for separation of concerns, so that the Space can be used as a synchronization
 * point without having to know about the entire world.
 * 
 * Extend this class to create your specific plugin.
 * 
 * ==Rules for SpacePlugins==
 * - Only use a SpacePlugin if you have a chunk of code that ''must'' happen inside the Space processing,
 *   SpacePlugins are automatically a little expensive, even if they are rarely invoked. They are
 *   mainly intended for crucial synchronization situations.
 * - The SpacePlugin's receive function must be '''fast''', and must '''never''' block. It may send
 *   messages, and handle responses, for asynchronous processing.
 * - The SpacePlugin must be self-contained. It will be owned by the Space itself, and will be discarded
 *   when the Space is unloaded.
 * - The receive method should avoid throwing Exceptions if possible; if they happen, they will force
 *   a reload of the Space, and the message will be dropped on the floor, as usual for receive.
 */
abstract class SpacePlugin(val space:SpaceAPI) {
  /**
   * The receive handler.
   * 
   * This will be plugged into the Space's receive pipeline, and offered messages that are not otherwise
   * dealt with.
   */
  def receive:Actor.Receive
  
  /**
   * The current sender. Valid during receive(), as usual.
   */
  def sender = space.context.sender
}

/**
 * This trait should be implemented by Ecots that want to plug their own code into all Spaces.
 */
trait SpacePluginProvider {
  /**
   * Creates a plugin for the given Space.
   */
  def createPlugin(space:SpaceAPI):SpacePlugin
}