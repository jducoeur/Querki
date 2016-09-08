package querki.spaces

import models.{Property, PType, PTypeBuilder, Thing, ThingState}
import models.Thing.PropMap

import querki.core.IsTextType
import querki.ecology._
import querki.util._
import querki.values._

import messages.SpaceMessage

case class TypeChangeInfo(typeChanged:Boolean, newType:PType[Any] with PTypeBuilder[Any, Any], serializedValues:Map[Thing, String],
    prop:Property[_,_])
{
  def finish(newProp:Property[_,_], stateIn:SpaceState, updateState:(SpaceState, Option[SpaceMessage]) => Unit)(implicit e:Ecology):Unit = {
    if (typeChanged) {
      // Now, run through all of the previously-serialized values, and rebuild them with the new Type:
      val newState = (stateIn /: serializedValues) { (state, oldPair) =>
        val (usingThing, serialized) = oldPair
        val usingThingId = usingThing.id
        val newV = newProp.deserialize(serialized)(state)
        val usingProps = usingThing.props + (prop.id -> newV)
        usingThing match {
          case t:ThingState => state.copy(things = state.things + (usingThingId -> t.copy(pf = usingProps)))
          case p:Property[_,_] => {
            state.copy(spaceProps = state.spaceProps + (usingThingId -> p.copy(pf = usingProps)))
          }
          case s:SpaceState => state.copy(pf = usingProps)
        }
      }
      
      updateState(newState, None)
    }    
  } 
}

private [spaces] trait PropTypeMigrator extends EcologyInterface {
  def checkLegalChange(state:SpaceState, oldThing:Thing, newProps:PropMap):Unit
  def prepChange(state:SpaceState, prop:Property[_,_], newProps:PropMap):TypeChangeInfo
}

object MOIDs extends EcotIds(25)

/**
 * This utility deals with the important edge case of trying to change the Type of a
 * Property. This is a sufficient big and complex problem as to demand its own class.
 * 
 * TODO (9/8/16): this Ecot is now Deprecated, and is not used in the Akka Persisted world.
 * It was always a fairly craptastic design, and doesn't fit well into the new code. See
 * Issue .7w4g7x4 for details on how this should be reimplemented later.
 */
class PropTypeMigratorEcot(e:Ecology) extends QuerkiEcot(e) with PropTypeMigrator {
  
  private def isLegalTypeChange(oldType:PType[_], newType:PType[_]):Boolean = {
    oldType.isInstanceOf[IsTextType] && newType.isInstanceOf[IsTextType]
  }
  
  /**
   * This will throw a PublicException iff the change is *not* legal.
   */
  def checkLegalChange(state:SpaceState, oldThing:Thing, newProps:PropMap):Unit = {
    implicit val s = state
    oldThing match {
      case prop:Property[_,_] => {
        for (
          oldTypeVal <- oldThing.getPropOpt(Core.TypeProp);
          oldTypeId <- oldTypeVal.firstOpt;
          newPropVal <- newProps.get(Core.TypeProp);
          newTypeId <- newPropVal.firstTyped(LinkType);
          if (oldTypeId != newTypeId);
          oldType <- state.typ(oldTypeId.toThingId);
          newType <- state.typ(newTypeId.toThingId);
          if (!isLegalTypeChange(oldType, newType))
            )
          throw new PublicException("Space.modifyThing.illegalTypeChange", oldType.displayName, newType.displayName)
        
        for (
          oldCollVal <- oldThing.getPropOpt(Core.CollectionProp);
          oldCollId <- oldCollVal.firstOpt;
          newCollVal <- newProps.get(Core.CollectionProp);
          newCollId <- newCollVal.firstTyped(LinkType);
          if (oldCollId != newCollId)
            )
          throw new PublicException("Space.modifyThing.illegalCollectionChange")
      }
      case _ =>
    }
  }
  
  def prepChange(state:SpaceState, prop:Property[_,_], newProps:PropMap):TypeChangeInfo = {
    implicit val s = state
    val propId = prop.id
    val newTypeOpt =
      for (
        newPropVal <- newProps.get(Core.TypeProp);
        newTypeId <- newPropVal.firstTyped(LinkType);
        newType <- state.typ(newTypeId.toThingId)
          )
        yield newType
    // TBD: same ugly cast as we have in SpacePersister. Is there a way around this?
    val newType = newTypeOpt.getOrElse(prop.pType).asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
    val changedTypes:Boolean = (newType.id != prop.pType.id)
        
    val serializedValues:Map[Thing, String] = {
      if (changedTypes) {
        // Save off all of the existing property values in serialized form:
        for (
          (usingThingId, usingThing) <- state.things;
          if (usingThing.props.contains(propId));
          oldPropAndVal <- usingThing.getPropOpt(propId)(state);
          oldVal = oldPropAndVal.v
            )
          yield (usingThing -> prop.serialize(oldVal))
      } else
        Map.empty
    }
    
    if (changedTypes)
      TypeChangeInfo(true, newType, serializedValues, prop)
    else
      TypeChangeInfo(false, prop.pType.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]], Map.empty, prop)
  }
}