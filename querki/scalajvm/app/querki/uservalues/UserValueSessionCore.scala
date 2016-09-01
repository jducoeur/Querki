package querki.uservalues

import models._
import Thing._

import querki.globals._
import querki.identity.{Identity, User}
import querki.persistence._
import querki.values.QValue

import PersistentEvents._

/**
 * This class abstracts out the concept of "my user values" from UserSpaceSession. It represents the UserValues in a given Space for
 * a specific User.
 * 
 * TODO: this probably eventually needs a proper snapshot.
 */
trait UserValueSessionCore extends PersistentActorCore with PersistentEvents with querki.types.ModelTypeDefiner with EcologyMember {
  
  private lazy val AccessControl = interface[querki.security.AccessControl]
  private lazy val UserValues = interface[querki.uservalues.UserValues]
  
  def rawState:SpaceState
  def identity:Identity
  /**
   * This is called for all changes that are *not* UserValues (that is, most ofthem).
   */
  def forwardNormalChanges(req:User, thingId:ThingId, props:PropMap):Unit
  /**
   * This is called for all changes that *are* UserValues, so that they can be denormalized to the
   * ThingUserValuesActor.
   */
  def forwardUserValue(req:User, uv:DHUserValue):Unit
  /**
   * This is called if a change calls for summarizing; it gets passed around to the Space itself.
   */
  def summarizeChange(req:User, change:SummarizeChange[_]):Unit
  
  /**
   * This person's UserValues. Index is (thingId, propId).
   */
  var userValues = Map.empty[(OID, OID), QValue]
  
  def doChangeProps(thingId:OID, uvProps:PropMap) = {
    uvProps.foreach { case (propId, v) =>
      userValues += ((thingId, propId) -> v)
    }    
  }

  /**
   * This tells the Space about the UserValues that need to be added to the summaries. It will only
   * call summarizeChange if this change is summarizable.
   */
  def sendSummaries(req:User, thingId:OID, uvProps:PropMap)(implicit state:SpaceState) = {
    uvProps.foreach { case (propId, v) =>
      val previous = userValues.get((thingId, propId))
      
      val msg = for {
        prop <- state.prop(propId) orElse QLog.warn(s"UserSpaceSession.ChangeProps2 got unknown Property $propId")
        summaryLinkPV <- prop.getPropOpt(UserValues.SummaryLink)
        summaryPropId <- summaryLinkPV.firstOpt
        newV = if (v.isDeleted) None else Some(v)
      }
        yield SummarizeChange(thingId, prop, summaryPropId, previous, newV)
      msg.map(summarizeChange(req, _))
    }        
  }
  
  /**
   * This takes a set of changes being proposed for the specified Thing, some of which may be User Values.
   * It deals with persisting the ones that are, and forwards the rest of the changes to be dealt with
   * elsewhere.
   */
  def changeProps(req:User, thingId:ThingId, props:PropMap):Unit = {
    
    implicit val s = rawState
    
    val (rawUvProps, nonUvProps) = props.partition { case (propId, v) => UserValues.isUserValueProp(propId) }
    
    // First, if there are non-UserValue changes (which is true about 99% of the time), pass them along:
    if (!nonUvProps.isEmpty) {
      forwardNormalChanges(req, thingId, nonUvProps)
    }
    
    // Now, deal with the UserValue changes.
    if (!rawUvProps.isEmpty) {
      rawState.anything(thingId) match {
        case Some(thing) => {
          //  First, make sure we limit ourselves to legal ones:
          val uvProps = rawUvProps.filter { case (propId, v) => AccessControl.hasPermission(UserValues.UserValuePermission, rawState, identity.id, thing.id) }
          if (!uvProps.isEmpty) {
            val uv = dh(identity.id, thing.id, uvProps)
            doPersist(uv) { _ =>
              // Note that sendSummaries() must come *before* doChangeProps, since it uses the old contents of
              // userValues:
              sendSummaries(req, thing.id, uvProps)
              doChangeProps(thing.id, uvProps)
              forwardUserValue(req, uv)
            }
          }
        }
        
        case None => QLog.warn(s"UserValueSessionCore got a UserValue change request for unknown Thing ${s.id}:$thingId")
      }
    }
//          
//    uvPropPairOpt match {
//      // It's a UserValue, so persist it that way:
//      case Some((propId, v)) => {
//        state.anything(thingId) match {
//          case Some(thing) => {
//            if (AccessControl.hasPermission(UserValues.UserValuePermission, state, identity.id, thing.id)) {
//              implicit val s = state
//              // Persist the change...
//              val uv = OneUserValue(identity, thing.id, propId, v, DateTime.now)
//              val previous = addUserValue(uv)
//   	          persister ! SaveUserValue(uv, state, previous.isDefined)
//   	          
//              // ... then tell the Space to summarize it, if there is a Summary Property...
//   	          val msg = for {
//   	            prop <- state.prop(propId) orElse QLog.warn(s"UserSpaceSession.ChangeProps2 got unknown Property $propId")
//   	            summaryLinkPV <- prop.getPropOpt(UserValues.SummaryLink)
//   	            summaryPropId <- summaryLinkPV.firstOpt
//   	            newV = if (v.isDeleted) None else Some(v)
//   	          }
//                yield SpacePluginMsg(req, spaceId, SummarizeChange(thing.id, prop, summaryPropId, previous, newV))
//              msg.map(spaceRouter ! _)
//                
//              // ... then tell the user we're set.
//              sender ! ThingFound(thing.id, state)
//            } else {
//              // Should we log a warning here? It *is* possible to get here, if the permission changed between the
//              // time the user loaded the page and the time they submitted it.
//              sender ! ThingError(new PublicException(ModifyNotAllowed))
//            }
//          }
//          case None => sender ! ThingError(UnexpectedPublicException, Some(state))
//        }
//      }
//      // It's not a UserValue, so just tell the Space about the change:
//      case None => spaceRouter.forward(ChangeProps(req, spaceId, thingId, props))
//    }    
  }
  
}