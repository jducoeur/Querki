package querki.types

import models.{DisplayPropVal, Kind, OID, Property, Thing, ThingState}
import models.Property.PropList
import models.Thing._

import models.system.{LinkType, NameType, PlainTextType}
import models.system.ExactlyOne
import models.system.{LinkAllowAppsProp, LinkModelProp, NameProp, SystemProperty}
import models.system.OIDs.{DisplayNameOID, NameOID, RootOID, systemOID}

import modules.Module

import querki.core.MOIDs.UrPropOID
import querki.ecology._
import querki.spaces.{SpaceChangeManager, ThingChangeRequest}

import querki.util._
import querki.values._

class DeriveNameModule(e:Ecology) extends Module(e) with DeriveName {
  
  import DeriveNameMOIDs._
  
  lazy val Basic = interface[querki.basic.Basic]
  
  override def init = {
    SpaceChangeManager.thingChanges += NameDeriver
  }
  
  override def term = {
    SpaceChangeManager.thingChanges += NameDeriver
  }
  
  private object NameDeriver extends Contributor[ThingChangeRequest, ThingChangeRequest] {
    // What is the appropriate derive flag to use for this request? 
    def deriveFlag(evt:ThingChangeRequest):OID = {
      implicit val s = evt.state
      val explicitFlagOpt =
        for (
          flagQV <- evt.newProps.get(DeriveNameProp);
          flagId <- flagQV.firstTyped(LinkType)
            )
        yield flagId
        
      explicitFlagOpt match {
        case Some(flag) => flag 
        case None => {
          // It wasn't in the input data, so look up the chain:
          val thing = evt.modelIdOpt.flatMap(s.anything(_)).getOrElse(Basic.SimpleThing)
          thing.firstOr(DeriveNameProp, DeriveInitiallyOID)          
        }
      }
    }
    
    // Given the Display Name, figure out what to actually name this Thing:
    def updateWithDerived(evt:ThingChangeRequest, displayName:String, kicker:Int = 0):ThingChangeRequest = {
      val candidate = NameType.makeLegal(displayName) + (if (kicker > 0) " " + kicker else "")
      val existingThing = evt.state.anythingByName(candidate)
      // It's a duplicate if we found the name *and* it isn't the Thing we're looking at:
      val isDuplicate = existingThing.isDefined && (evt.thingOpt.isEmpty || evt.thingOpt.get.id != existingThing.get.id)
      if (isDuplicate)
        // Recurse until we find a name not in use:
        updateWithDerived(evt, displayName, kicker + 1)
      else
        evt.copy(newProps = evt.newProps + NameProp(candidate))
    }

    // This is called whenever we get a Create or Modify request.
    def notify(evt:ThingChangeRequest, sender:Publisher[ThingChangeRequest,ThingChangeRequest]):ThingChangeRequest = {
      evt.thingOpt match {
        // Don't even try this on Spaces -- it's too dangerous:
        case Some(SpaceState(_, _, _, _, spaceName, _, _, _, _, _, _, _)) => {
          if (evt.newProps.contains(NameOID))
            evt
          else
            evt.copy(newProps = evt.newProps + NameProp(spaceName))
        }
        case _ => {
          implicit val s = evt.state
          val oldNameOpt = evt.thingOpt.flatMap(_.getPropOpt(NameProp)).flatMap(_.firstOpt)
          val existingNameOpt = evt.newProps.get(NameProp).flatMap(_.firstTyped(NameType)).orElse(oldNameOpt)
          // The fallback default: make sure that evt contains a sensible Name if possible. Since it may not be showing in the
          // Editor, we have to fill it back in from the existing Name in many cases:
          def evtWithExistingName = existingNameOpt.map(existingName => evt.copy(newProps = evt.newProps + NameProp(existingName))).getOrElse(evt)
          
	      val newDisplayNameOpt = for (
	        newDisplayNameVal <- evt.newProps.get(DisplayNameOID);
	        name <- newDisplayNameVal.firstTyped(PlainTextType)
	          )
	        yield name.text;
	      
	      newDisplayNameOpt match {
	        case Some(displayName) => {
	          val flag = deriveFlag(evt)
	          flag match {
	            case DeriveInitiallyOID => {
	              if (existingNameOpt.isDefined && existingNameOpt.get.length() > 0)
	                evtWithExistingName
	              else
	                // Only derive the new Name iff it's currently empty
	                updateWithDerived(evt, displayName)
	            }
	            
	            case DeriveAlwaysOID => updateWithDerived(evt, displayName)
	            
	            case DeriveNeverOID => evtWithExistingName
	            
	            case _ => evtWithExistingName
	          }          
	        }
	        
	        // If there's no Display Name, don't do anything
	        case None => evtWithExistingName
	      }          
        }
      }
    }
  }
  
  private def isProperty(model:Thing)(implicit state:SpaceState):Boolean = {
    model.id == UrPropOID || model.isAncestor(UrPropOID)
  }
  
  /**
   * Used from the Editor UI, and intended to be used in a filter of Properties. Strips out NameProp unless
   * we aren't deriving it.
   */
  def filterNameIfDerived(state:SpaceState, model:Thing, props:PropList, propPair:(Property[_,_], DisplayPropVal)):Boolean = {
    implicit val s = state
    val (prop, displayPropVal) = propPair
    if (prop == NameProp && !isProperty(model)) {
      val derivedVal:OID = {
        val localVal:Option[OID] = props.get(DeriveNameProp).flatMap(_.v).flatMap(_.firstTyped(LinkType))
        localVal.getOrElse(model.firstOr(DeriveNameProp, DeriveInitially.id))
      }
      derivedVal == deriveNever.id
    } else
      true
  }
  
  /******************************************
   * THINGS
   ******************************************/
  
  lazy val deriveModel = new ThingState(DeriveModelOID, systemOID, RootOID,
    toProps(
      setName("_deriveNameModel")))
  
  lazy val deriveAlways = new ThingState(DeriveAlwaysOID, systemOID, deriveModel,
    toProps(
      setName("Always Derive Name")))
  
  lazy val DeriveInitially = new ThingState(DeriveInitiallyOID, systemOID, deriveModel,
    toProps(
      setName("Derive Name Initially")))
  
  lazy val deriveNever = new ThingState(DeriveNeverOID, systemOID, deriveModel,
    toProps(
      setName("Never Derive Name")))
  
  override lazy val things = Seq(
    deriveModel,
    deriveAlways,
    DeriveInitially,
    deriveNever
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  lazy val DeriveNameProp = new SystemProperty(DeriveNameOID, LinkType, ExactlyOne,
    toProps(
      setName("_deriveName"),
      LinkModelProp(deriveModel),
      LinkAllowAppsProp(true),
      SkillLevel(SkillLevel.Advanced),
      Summary("Should this Thing's Name be automatically derived from its Display Name?"),
      Details("""Querki has two different kinds of "name". Both are important, but they are
          |very different:
          |
          |* A Thing's Name is its official name -- the one that shows up in its URL, and which you use to refer to it in QL expressions.
          |* A Thing's Display Name is what actually gets *shown* most of the time.
          |
          |The most important difference is that Name is very restricted: it can contain letters, digits and spaces, and nothing
          |else. It can't contain any of the punctuation you often want in a name: apostrophes, dashes, and stuff like that. So you
          |usually want to think about the Display Name, not the Name.
          |
          |However, the Name is still important, so Querki will fill it in for you unless you tell it not to. When we talk about
          |"deriving" the Name, we mean Querki looking at the Display Name and removing all the characters it can't deal with, to
          |get the Name. Sometimes the result is exactly the same as Display Name, sometimes not, although a derived Name will usually
          |be pretty close to the Display Name.
          |
          |This Property says whether to do so. It has three possible settings, which are appropriate for different situations and tastes:
          |
          |* *Derive Name Initially* means that the Name will be set based on the Display Name the *first* time you save it, but
          |will stay that Name thereafter. This means that, if you change the Display Name, it might wind up quite different from
          |the Name. But since the Name doesn't change, it means that references to this Thing are less likely to break. This is
          |the default, and in most cases you can just leave it this way.
          |* *Always Derive Name* means that the Name will change whenever the Display Name does. This makes it easier to remember
          |what the Name is, but means that if you have references to this Thing -- if you have put its URL somewhere, or refer
          |to it in a QL Expression, that might wind up as a broken link if you ever change the Display Name.
          |* *Never Derive Name* means that you will set the Name (or not) yourself: Querki shouldn't do anything automatically.
          |This gives you maximum control, but is the most work.""".stripMargin)))
  
  override lazy val props = Seq(
    DeriveNameProp
  )
}
