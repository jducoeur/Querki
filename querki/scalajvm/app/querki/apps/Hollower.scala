package querki.apps

import akka.actor._

import org.querki.requester._

import models._
import Thing.PropMap

import querki.globals._
import querki.identity.User
import querki.spaces.SpaceBuilder.{IDMap, NewSpaceInfo}
import querki.spaces.messages._

/**
 * The part of the Extract App process that "hollows out" the extracted items from the original Space.
 * This is run after we've actually created the new App: it goes through all the extracted Things,
 * points them at their App representations, and removes the duplicate Property Values.
 * 
 * This is separated out solely to keep file sizes under control. Conceptually, it's part of ExtractAppActor.
 * 
 * @author jducoeur
 */
private [apps] trait Hollower { self:Actor with Requester with EcologyMember =>
  
  /**
   * Syntax sugar for building purely side-effecting chains of requests. This is as un-pure as
   * you can get, but makes sense in the Akka context.
   * 
   * TODO: once we are confident this works, it should get extracted to Requester and properly
   * unit-tested.
   */
  implicit class UnitRequest(req:RequestM[Unit]) {
    /**
     * Do something after this Request is finished.
     * 
     * TODO: this probably isn't the right name, because it has different meaning in Future.
     * What should we call it?
     */
    def andThen(fNext: => RequestM[Unit]):RequestM[Unit] = req flatMap { dummy => fNext }
  }
  
  def Apps:querki.apps.Apps
  def Core:querki.core.Core
  def Types:querki.types.Types
  
  def setMsg(msg:String):Unit
  def withMsg[R](msg:String, f: => R):R
  
  def state:SpaceState
  def owner:User
  def router:ActorRef
  def SystemSpace:SpaceState
  
  def spaceId = state.id
  
  def hollowSpace(extractees:Extractees, appInfo:NewSpaceInfo):RequestM[Unit] = {
    setMsg("Rewriting the Space")
    
    implicit val s = state
    
    val idMap = appInfo.mapping
    val (models, instances) = extractees.state.things.values.partition(_.isModel)
    
    withMsg("Removing extracted Instances", deleteInstances(instances)) andThen 
      withMsg("Adjusting Models", hollowThings(models, idMap, {(tpe, idMap) => Map.empty})) andThen
      withMsg("Adjusting Types", hollowThings(extractees.state.types.values, idMap, mapTypeModel)) andThen
      withMsg("Adjusting Properties", hollowThings(extractees.state.spaceProps.values, idMap, mapPropType)) andThen
      hollowSpace(extractees, idMap)
  }
  
  /**
   * The only Props we do *not* filter are the ones that have the NotInherited flag.
   * 
   * We precalculate this, to keep the hollowing process as fast as possible.
   * 
   * Note the explicit assumption that NotInherited only applies to System properties,
   * and that we should inherit everything *except* those Properties.
   */
  val uninheritedProps =
    SystemSpace
      .spaceProps
      .filter { pair =>
        val (k, prop) = pair
        prop.props.contains(querki.core.MOIDs.NotInheritedOID)
      }
      .keys
      .toSet
  
  def hollowThing(thing:Thing):PropMap = {
    // Remove all props on this Thing *except* the ones that are unique to it, and mark it as
    // a Shadow:
    thing.props.filterKeys(uninheritedProps.contains(_)) + Apps.ShadowFlag(true)
  }
      
  def mapTypeModel(tpe:Thing, idMap:IDMap):PropMap = {
    implicit val s = state
    val resultOpt = for {
      oldModelPV <- tpe.getPropOpt(Types.ModelForTypeProp)
      oldModelId <- oldModelPV.firstOpt
      newModelId <- idMap.get(oldModelId)
    }
      yield Map(Types.ModelForTypeProp(newModelId))
      
    resultOpt.getOrElse(Map.empty)
  }
  
  def mapPropType(prop:Thing, idMap:IDMap):PropMap = {
    implicit val s = state
    val resultOpt = for {
      oldTypePV <- prop.getPropOpt(Core.TypeProp)
      oldTypeId <- oldTypePV.firstOpt
      newTypeId <- idMap.get(oldTypeId)
    }
      yield Map(Core.TypeProp(newTypeId))
      
    resultOpt.getOrElse(Map.empty)    
  }
  
  // We use a single function for all the hollowing-out, since it is basically the same for everything. The
  // only real difference is that some Kinds have a "kicker" that is slightly different, which is the addl parameter.
  def hollowThings(things:Iterable[Thing], idMap:IDMap, addl:(Thing, IDMap) => PropMap):RequestM[Unit] = {
    things.headOption match {
      case Some(thing) => {
        router.request(ModifyThing(owner, spaceId, thing.id, idMap(thing.id), hollowThing(thing) ++ addl(thing, idMap))) flatMap {
          case found @ ThingFound(_, _) => {
            hollowThings(things.tail, idMap, addl)
          }
          case ThingError(ex, _) => {
            RequestM.failed(ex)
          }
        }
      }
      case None => RequestM.successful(())
    }
  }
  
  def hollowSpace(extractees:Extractees, idMap:IDMap):RequestM[Unit] = {
    // Extracting the Space itself is optional; did we do so?
    if (extractees.extractState) {
      withMsg("Adjusting Space", hollowThings(Seq(state), idMap, {(tpe, idMap) => Map.empty}))
    } else
      RequestM.successful(())
  }
  
  /**
   * All Instances that were extracted simply get removed from the child Space.
   */
  def deleteInstances(things:Iterable[Thing]):RequestM[Unit] = {
    things.headOption match {
      case Some(thing) => {
        router.request(DeleteThing(owner, spaceId, thing.id)) flatMap {
          case found @ ThingFound(_, _) => {
            deleteInstances(things.tail)
          }
          case ThingError(ex, _) => {
            RequestM.failed(ex)
          }
        }        
      }
      case None => RequestM.successful(())
    }
  }
}