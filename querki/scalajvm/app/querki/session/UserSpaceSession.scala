package querki.session

import akka.actor._
import akka.persistence._
import models._
import querki.globals._
import querki.identity._
import querki.persistence._
import querki.spaces.messages.{ChangeProps, SpacePluginMsg}
import querki.uservalues._
import querki.uservalues.PersistentEvents._
import querki.values.RequestContext

/**
 * This PersistentActor represents the relationship of this User to this Space.
 * 
 * It encapsulates and persists the User Values for this User in this Space.
 * 
 * It creates the filtered view of the SpaceState, by removing anything this User isn't
 * allowed to see and adding their User Values.
 * 
 * It owns several of the major Autowire API implementations; executing those entry points
 * happens in the context of this Actor.
 * 
 * TODO: at this point, this is just an initial skeleton. We need to flesh out UserValueSessionCore
 * and this, to contain all of the functionality from the OldUserSpaceSession.
 */
class UserSpaceSession(val ecology:Ecology, initState:SpaceState, val user:User, val spaceRouter:ActorRef)
  extends UserValueSessionCore with PersistentQuerkiActor with querki.types.ModelTypeDefiner 
{
  lazy val Person = interface[querki.identity.Person]
  
  val spaceId = initState.id
  
  /**
   * The "raw" state, before adding User Values.
   */
  var rawState:SpaceState = initState
  
  val identity = Person.localIdentities(user)(rawState).headOption.getOrElse(user.mainIdentity)
  
  val persistenceId = s"uss-${spaceId.toString}-${identity.id.toString}"
  
  /**
   * This is called for all changes that are *not* UserValues (that is, most ofthem).
   */
  def forwardNormalChanges(rc: RequestContext, thingId:ThingId, props:PropMap):Unit = {
    spaceRouter.forward(ChangeProps(rc, spaceId, thingId, props))
  }
  
  /**
   * This is called for all changes that *are* UserValues, so that they can be denormalized to the
   * ThingUserValuesActor.
   */
  def forwardUserValue(req:User, uv:DHUserValue):Unit = {
    // TODO: this needs to create a new message, containing the DHUserValue, and send it along.
    // The ThingUserValuesActor for this Thing then persists the value so that one can easily
    // look at the Thing and get a hold of all of the UserValues for it.
  }
  
  /**
   * This is called if a change calls for summarizing; it gets passed around to the Space itself.
   */
  def summarizeChange(rc: RequestContext, change:SummarizeChange[_]):Unit = {
    spaceRouter ! SpacePluginMsg(rc, spaceId, change)
  }
  
  def receiveRecover = {
    // TODO:
    case _ => ???
  }
  
  def receiveCommand = {
    // TODO:
    case _ => ???
  }
}