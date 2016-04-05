package querki.types

import models.{DisplayPropVal, Kind, OID, Property, Thing, ThingState}

import querki.api.commonName
import querki.core.{NameUtils, PropList}
import querki.core.MOIDs.UrPropOID
import querki.ecology._
import querki.spaces.{SpaceChangeManager, TCRReq, ThingChangeRequest}

import querki.util._
import querki.values._

class DeriveNameModule(e:Ecology) extends QuerkiEcot(e) with DeriveName with NameUtils {
  
  import DeriveNameMOIDs._
  
  val Links = initRequires[querki.links.Links]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  lazy val Basic = interface[querki.basic.Basic]
  
  lazy val NameProp = Core.NameProp
  
  override def init = {
    SpaceChangeManager.thingChanges += NameDeriver
  }
  
  override def term = {
    SpaceChangeManager.thingChanges += NameDeriver
  }
  
  private object NameDeriver extends Contributor[TCRReq, TCRReq] {
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
      val candidate = makeLegal(displayName) + (if (kicker > 0) " " + kicker else "")
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
    def notify(evtReq:TCRReq, sender:Publisher[TCRReq,TCRReq]):TCRReq = {
      evtReq.map { evt =>
        evt.thingOpt match {
          // Don't even try this on Spaces -- it's too dangerous:
          case Some(state:SpaceState) => {
            if (evt.newProps.contains(querki.core.MOIDs.NameOID))
              evt
            else
              evt.copy(newProps = evt.newProps + NameProp(state.name))
          }
          case _ => {
            implicit val s = evt.state
            val oldNameOpt = evt.thingOpt.flatMap(_.getPropOpt(NameProp)).flatMap(_.firstOpt)
            val existingNameOpt = evt.newProps.get(NameProp).flatMap(_.firstTyped(Core.NameType)).orElse(oldNameOpt)
            // The fallback default: make sure that evt contains a sensible Name if possible. Since it may not be showing in the
            // Editor, we have to fill it back in from the existing Name in many cases:
            def evtWithExistingName = existingNameOpt.map(existingName => evt.copy(newProps = evt.newProps + NameProp(existingName))).getOrElse(evt)
            
    	      val newDisplayNameOpt = for (
    	        newDisplayNameVal <- evt.newProps.get(querki.basic.MOIDs.DisplayNameOID);
    	        name <- newDisplayNameVal.firstTyped(Basic.PlainTextType)
    	          )
    	        yield name.text;
    	      
    	      newDisplayNameOpt match {
              // Only bother if we can get a non-empty Link Name out of this:
    	        case Some(displayName) if (makeLegal(displayName).length > 0) => {
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
    	        case _ => evtWithExistingName
    	      }    
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
      derivedVal == DeriveNever.id
    } else
      true
  }
  
  def nameIsDerived(thing:Thing, state:SpaceState):Boolean = {
    implicit val s = state
    val resultOpt = for (
      propAndVal <- thing.getPropOpt(DeriveNameProp);
      flag <- propAndVal.firstOpt
        )
      yield flag != DeriveNever.id
      
    resultOpt.getOrElse(true)
  }
  
  /******************************************
   * THINGS
   ******************************************/
  
  lazy val deriveModel = new ThingState(DeriveModelOID, systemOID, RootOID,
    toProps(
      setName("_deriveNameModel")))
  
  lazy val DeriveAlways = new ThingState(DeriveAlwaysOID, systemOID, deriveModel,
    toProps(
      setName(commonName(_.types.deriveAlways))))
  
  lazy val DeriveInitially = new ThingState(DeriveInitiallyOID, systemOID, deriveModel,
    toProps(
      setName("Derive Name Initially")))
  
  lazy val DeriveNever = new ThingState(DeriveNeverOID, systemOID, deriveModel,
    toProps(
      setName(commonName(_.types.deriveNever))))
  
  override lazy val things = Seq(
    deriveModel,
    DeriveAlways,
    DeriveInitially,
    DeriveNever
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  lazy val DeriveNameProp = new SystemProperty(DeriveNameOID, LinkType, ExactlyOne,
    toProps(
      setName(commonName(_.types.deriveNameProp)),
      Links.LinkModelProp(deriveModel),
      Links.LinkAllowAppsProp(true),
      setInternal,
      SkillLevel(SkillLevelAdvanced),
      Summary("Should this Thing's Link Name be automatically derived from its Name?"),
      Details("""Querki has two different kinds of "name". Both are important, but they are
          |very different:
          |
          |* A Thing's Link Name is its official name -- the one that shows up in its URL, and which you use to refer to it in QL expressions.
          |* A Thing's Name is what actually gets *shown* most of the time.
          |
          |The most important difference is that Link Name is very restricted: it can contain letters, digits and spaces, and nothing
          |else. It can't contain any of the punctuation you often want in a name: apostrophes, dashes, and stuff like that. So you
          |usually want to think about the Name, not the Link Name.
          |
          |However, the Link Name is still important, so Querki will fill it in for you unless you tell it not to. When we talk about
          |"deriving" the Name, we mean Querki looking at the Name and removing all the characters it can't deal with, to
          |get the Link Name. Sometimes the result is exactly the same as Name, sometimes not, although a derived Link Name will usually
          |be pretty close to the Name.
          |
          |This Property says whether to do so. It has three possible settings, which are appropriate for different situations and tastes:
          |
          |* *Derive Name Initially* means that the Link Name will be set based on the Name the *first* time you save it, but
          |will stay that Link Name thereafter. This means that, if you change the Name, it might wind up quite different from
          |the Link Name. But since the Link Name doesn't change, it means that references to this Thing are less likely to break. This was
          |the default, but is now deprecated: it proves too confusing in practice.
          |* *Always Derive Name* means that the Link Name will change whenever the Name does. This makes it easier to remember
          |what the Link Name is, but means that if you have references to this Thing -- if you have put its URL somewhere, or refer
          |to it in a QL Expression, that might wind up as a broken link if you ever change the Name. This is now the default.
          |* *Never Derive Name* means that you will set the Link Name (or not) yourself: Querki shouldn't do anything automatically.
          |This gives you maximum control, but is more work.
          |
          |It is rare to set this property by hand. Instead, use the "Derive the Link Name from the Name" checkbox in the
          |Advanced Editor. This is usually checked, which means *Always Derive* -- the Link Name will change when the Name
          |does. If you uncheck it, that means *Never Derive* -- the Link Name will no longer change. Unchecking it makes
          |sense if you might have outside pages pointing to this Thing by its Link Name, and don't want to break those
          |pointers.""".stripMargin)))
  
  override lazy val props = Seq(
    DeriveNameProp
  )
}
