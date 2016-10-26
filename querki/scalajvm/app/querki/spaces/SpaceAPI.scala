package querki.spaces

import akka.actor._

import org.querki.requester._

import models.{Kind, OID, Thing, ThingId}
import models.Thing.PropMap

import querki.identity.User
import querki.persistence.{PersistentActorCore, UseKryo}
import querki.spaces.messages.SpaceMessage
import querki.values.{SpaceState, SpaceVersion}

/**
 * The abstraction of a Space, so that SpacePlugins can interact with it.
 * 
 * IMPORTANT: this must NEVER be used outside of a SpacePlugin, when that Plugin has been properly
 * invoked by the Space itself! Anything else is incredibly dangerous!
 */
trait SpaceAPI[RM[_]] extends PersistentActorCore {
  /**
   * Accessor to the current state of this Space.
   */
  def state:SpaceState
  
  /**
   * Actually changes the Space. Usually called *inside* persistMsgAPI(), and will execute after
   * the message is persisted.
   */
  def updateAfter(f: SpaceState => SpaceState):SpaceState
  
  /**
   * Persists the specified events, and lets you hook into what happens afterwards.
   */
  def persistAllAnd(events:collection.immutable.Seq[UseKryo]):RM[Seq[UseKryo]]
  
  /**
   * DEPRECATED: this tried to do too much at once. We're using different patterns now.
   * 
   * This is how a plugin tells the system to persist an event. The handler should generally
   * be a call to updateAfter().
   * 
   * This will respond with a ThingFound(oid) if all is successful.
   */
  def persistMsgThen[A <: UseKryo, R](oid:OID, event:A, handler: => R, sendAck:Boolean = true):RM[R]
  
  /**
   * DEPRECATED: Record a change to the specified Thing.
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
  def modifyThing(who:User, thingId:ThingId, modelIdOpt:Option[OID], pf:(Thing => PropMap), sync:Boolean):Unit
  
  /**
   * Create a new Thing.
   * 
   * IMPORTANT: this is strictly for internal use, and does *not* do as much massaging as usual!
   * 
   * @param sendAck Iff true, the usual ThingFound message will be sent to sender.
   */
  def doCreate(who:User, modelId:OID, props:PropMap, kind:Kind.Kind, sendAck:Boolean)(state:SpaceState):RM[ChangeResult]
  
  /**
   * The newer and better way to modify a Thing.
   */
  def modifyThing(who:User, thingId:ThingId, modelIdOpt:Option[OID], rawNewProps:PropMap, replaceAllProps:Boolean, sendAck:Boolean)(state:SpaceState):RM[SpaceState]
  
  def loadAppVersion(appId:OID, version:SpaceVersion, appsSoFar:Map[OID, SpaceState]):RM[SpaceState]
}

/**
 * A SpacePlugin allows external Ecots to add specialized messages that each Space can handle.
 * This is mainly for separation of concerns, so that the Space can be used as a synchronization
 * point without having to know about the entire world.
 * 
 * Extend this class to create your specific plugin.
 * 
 * Note that this takes an RM type parameter -- this is the monadic abstraction of RequestM, and *must*
 * be used instead of RequestM. In return, your Plugin can work in synchronous unit tests.
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
abstract class SpacePlugin[RM[_]](val space:SpaceAPI[RM], rtc:RTCAble[RM]) {
  /**
   * The receive handler.
   * 
   * This will be plugged into the Space's receive pipeline, and offered messages that are not otherwise
   * dealt with.
   */
  def receive:Actor.Receive
}

/**
 * This trait should be implemented by Ecots that want to plug their own code into all Spaces.
 * 
 * TODO: the typeclasses here are *all* messed up -- I'm sure we are tying ourselves far too deeply
 * in knots. This needs rationalization!
 */
trait SpacePluginProvider {
  /**
   * Creates a plugin for the given Space.
   */
  def createPlugin[RM[_]](space:SpaceAPI[RM], rtc:RTCAble[RM]):SpacePlugin[RM]
}
