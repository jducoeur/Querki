package querki

import scala.concurrent.Future

import akka.actor.ActorRef

import anorm.SqlQuery

import org.querki.requester._

import models._

import querki.ecology._
import querki.identity.User
import querki.spaces.messages.{SpaceMessage, SpaceMgrMsg}
import querki.time.DateTime
import querki.util.{Aggregator, Sequencer}
import querki.values.{QValue, SpaceState, StateCacheKey}

package object spaces {
  
  /**
   * Represents the states that a given Space can be in.
   */
  case class SpaceStatusCode(val underlying:Int) extends AnyVal
  /**
   * Normal status: this Space is alive, visible and functioning.
   */
  final val StatusNormal = SpaceStatusCode(0)
  /**
   * This Space has been Archived, and can not be used.
   */
  final val StatusArchived = SpaceStatusCode(1)
  /**
   * This Space is currently being built, and should not be loaded conventionally.
   */
  final val StatusInitializing = SpaceStatusCode(2)
  
  // This is a pure marker trait, indicating that this PropValue didn't load correctly yet:
  trait UnresolvedPropValue

  // The name of the Space Actor
  def sid(id:OID):String = id.toString
  
  /**
   * Represents a serialized PropMap.
   */
  case class SerializedProps(val ser:String) extends AnyVal
  
  trait SpaceOps extends EcologyInterface {
    /**
     * Fetch a reference to a SpaceManager Actor, to which you can send requests. This is the
     * only legal entry point to the Space Management system from Play! 
     */
    def spaceManager:ActorRef
    
    def spaceRegion:ActorRef
    
    /**
     * Given a string representation of a Space (which we presume is a ThingId), returns the
     * actual OID of that Space. Will fail the Future if the ID doesn't represent an actual
     * Space. Requires that you have already looked up the owner.
     */
    def getSpaceId(ownerId:OID, spaceId:String):Future[OID]
    
    /**
     * Finds out whether this name is currently used as a Space by this User.
     */
    def spaceExists(ownerId:OID, spaceId:String):Future[Boolean]
    
    /**
     * Send a message to the SpaceManager, expecting a return of type A to be 
     * passed into the callback. This wraps up the messy logic to go from a
     * non-actor-based Play environment to the SpaceManager. We'll likely
     * generalize it further eventually.
     *
     * Type A is the response we expect to get back from the message, which will
     * be sent to the given callback.
     *
     * Type B is the type of the callback. I'm a little surprised that this isn't
     * inferred -- I suspect I'm doing something wrong syntactically.
     */ 
    def askSpaceManager[A,B](msg:SpaceMgrMsg)(cb: A => Future[B])(implicit m:Manifest[A]):Future[B]
    
    /**
     * Simplified version of ask, which doesn't try to be excessively clever. This
     * one has a small disadvantage: it doesn't provide compile-time thoroughness checking
     * of the results. But on the plus side, it copes cleanly with errors from the back
     * end, which the older ask() does not.
     */
    def askSpaceManager2[B](msg:SpaceMgrMsg)(cb: PartialFunction[Any, Future[B]]):Future[B]
    
    def askSpace[A, B](msg:SpaceMessage)(cb: A => Future[B])(implicit m:Manifest[A]):Future[B]
    def askSpace2[B](msg:SpaceMessage)(cb: PartialFunction[Any, Future[B]]):Future[B]

    /**
      * This gets called when a SpaceState gets fully evolved, and sends out a notification to listeners. It exists
      * *solely* for unit testing, and should not be used for anything else.
      */
    def notifyFullEvolution(): Unit
    def registerFullEvolutionListener(f: () => Unit): Unit
  }
    
  trait SpacePersistence extends EcologyInterface {
    def UnresolvedPropType:PType[String]
    /**
     * This notes a serialized Property value that we're trying to deserialize but can't yet.
     * 
     * TODO: this arguably no longer belongs in SpacePersistence, but instead in querki.persistence
     * somewhere. Think about the factoring.
     */
    def recordUnresolvedProp(valStr:String):QValue
    
    // The name of the Space's Thing Table
    def thingTable(id:OID):String
    
    def SpaceSQL(spaceId:OID, query:String, version:Int = 0):SqlQuery
    
    // More type-safe versions of serializeProps and deserializeProps:
    def serProps(props:PropMap, space:SpaceState):SerializedProps = {
      SerializedProps(serializeProps(props, space))
    }
    def deserProps(ser:SerializedProps, space:SpaceState):PropMap = {
      deserializeProps(ser.ser, space)
    }

    // Deprecated -- prefer the type-safe versions above:
    def serializeProps(props:PropMap, space:SpaceState):String
    def deserializeProps(str:String, space:SpaceState):PropMap
    def deserializeProp(propStr:String)(implicit space:SpaceState):(OID, QValue)
    def createThingInSql(thingId:OID, spaceId:OID, modelId:OID, kind:Int, props:PropMap, modTime:DateTime, serialContext:SpaceState)(implicit conn:java.sql.Connection):Int
  }
    
  // All of the information that is passed into listeners when a change happens.
  // This structure is getting a tad crazy-large. I suspect there are abstractions fighting
  // to break out of it.
  case class ThingChangeRequest(
      // Who is requesting this change
      who:User,
      // The Requester to use, if we need to loop stuff back:
      req:Requester,
      // The current state. Note that this can potentially change before all TCR handlers return!
      state:SpaceState, 
      // The router at the head of the troupe, if it is necessary to do anything with other Actors
      router:ActorRef,
      // The Model, iff it is being changed
      modelIdOpt:Option[OID], 
      // The existing Thing, if this is a change; None, if this is a Create operation
      thingOpt:Option[Thing], 
      // The Kind of the Thing
      kind:Kind.Kind, 
      // The full Properties that we are proposing to set on this Thing. Listeners may alter this
      newProps:PropMap, 
      // The Properties that are being changed by this operation
      changedProps:Seq[OID])
  
  type TCRReq = RequestM[ThingChangeRequest]
  
  case class CacheUpdate(evt:Option[querki.spaces.messages.SpaceMessage], old:Option[SpaceState], current:SpaceState) {
    /**
     * Listeners to updateStateCache should usually call this at the end to update the cache with their particular value.
     */
    def updateCacheWith(ecotId:Short, key:String, v:Any):CacheUpdate = {
      copy(current = current.copy(cache = current.cache + (StateCacheKey(ecotId, key) -> v)))
    }
  }

  trait SpaceChangeManager extends EcologyInterface {
    /**
     * Called before every Create or Modify operation. Listeners can use this specifically to edit the Props.
     * 
     * IMPORTANT: this contains a RequestM. It is legal for listeners to invoke long-running operations, but they
     * must happen in *other* Actors. Note that this may result in other things happening in this Space while
     * those occur, so be careful when thinking about ordering here!
     */
    def thingChanges:Sequencer[TCRReq]
    
    /**
     * Called whenever the state changes. Listeners should update their cache entries in the "current" field.
     * 
     * IMPORTANT: this is called by the Space with every change. It much not block, and it must be decently
     * efficient. Ecots should use the cache when they have information that is (a) used frequently (more than
     * once per State change on average); (b) moderately expensive to calculate that often; (c) based strictly
     * on the SpaceState. Do *not* over-use it, but sometimes it's just the thing.
     */
    def updateStateCache:Sequencer[CacheUpdate]
    
    /**
     * Allows other Ecots to plug their own code into Space processing.
     */
    def registerPluginProvider(provider:SpacePluginProvider)
    
    /**
     * Lets a Space get at all the SpacePluginProviders. Should not be used outside of Spaces.
     */
    def spacePluginProviders:Seq[SpacePluginProvider]
  }
}