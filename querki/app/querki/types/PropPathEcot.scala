package querki.types

import models.{Property, PropertyBundle}

import querki.ecology._
import querki.values.{PropAndVal, SpaceState}
  
/**
 * A way to get to a Property, which might be contained in Model Properties. Most important for
 * the getPropOpt() method, which is similar to Thing.getPropOpt(prop), but in this case is
 * Path.getPropOpt(thing) -- what property values does this Path return for this Thing.
 */
sealed trait PropPath[VT,WT] {
  /**
   * The actual underlying Property that this is all about.
   */
  def prop:Property[VT,_]
  /**
   * The Property we are looking for while we build things out.
   */
  def lookingUp:Property[WT,_]
    
  /**
   * Actually look up this Property value via this path. Note that this can return multiple values
   * if, for instance, the Model Property in the middle is List-valued.
   */
  def getPropOpt(thing:PropertyBundle)(implicit state:SpaceState):Seq[PropAndVal[VT]]
}
case class TrivialPropPath[VT](prop:Property[VT,_]) extends PropPath[VT,VT] {
  def lookingUp = prop
  def getPropOpt(thing:PropertyBundle)(implicit state:SpaceState):Seq[PropAndVal[VT]] = thing.getPropOpt(prop).toSeq
}
case class ContainedPropPath[VT](container:Property[ModeledPropertyBundle,_], path:PropPath[VT,_]) extends PropPath[VT,ModeledPropertyBundle] {
  def lookingUp = container
  def prop = path.prop
  def getPropOpt(thing:PropertyBundle)(implicit state:SpaceState):Seq[PropAndVal[VT]] = {
    thing.getPropOpt(container) match {
      case Some(contPV) => {
        val wrappedValues = for {
          containerElem <- contPV.rawList
        }
          yield path.getPropOpt(containerElem)
          
        wrappedValues.flatten
      }
      case None => Seq.empty
    }
  }
}

/**
 * TBD: is it worth pre-calculating all of the indirect paths, and stuffing them into the State Cache?
 * Model Types are unusual enough that it wouldn't be an enormous overhead, and it would reduce PathsToProperty
 * to more or less O(1) instead of the monstrosity it now is.
 */
class PropPathEcot(e:Ecology) extends QuerkiEcot(e) with PropPaths {

  /**
   * Given a particular Property's path, this returns all of paths that can get to it from Model Properties.
   * 
   * Note that this is recursive, because Model Properties can contain deeper Model Properties.
   */
  def modelPropertiesWithPath[VT](allModelTypes:Iterable[ModelTypeBase], wrapping:PropPath[VT,_])(implicit state:SpaceState):Iterable[PropPath[VT,ModeledPropertyBundle]] = {
    val prop = wrapping.lookingUp
    val modelTypes = for {
      modelType <- allModelTypes
      modeledType <- state.anything(modelType.basedOn)
      if modeledType.hasProp(prop)
    }
      // Okay, this Type's model contains the Property we are looking for.
      yield modelType
      
    val paths = for {
      modelType <- modelTypes
      candidateProp <- state.propList
      propOfType <- candidateProp.confirmType(modelType)
      path = ContainedPropPath(propOfType, wrapping)
    }
      yield path
      
    val indirectPaths = paths.map(modelPropertiesWithPath(allModelTypes, _)).flatten
      
    paths ++ indirectPaths
  }
  
  /**
   * This produces all of the ways to get *to* this Property in this Space, both directly and via Model Types.
   * 
   * TODO: now that the State Cache exists, it is probably worthwhile to simply precompute and cache *all* non-trivial paths
   * to *all* Properties, making this an O(1) operation the rest of the time. The precomputation wouldn't be
   * terribly hard: just run down all of the Model Properties and build paths to everything in them.
   */
  def pathsToProperty[VT](prop:Property[VT,_])(implicit state:SpaceState):Seq[PropPath[VT,_]] = {
    val allModelTypes = for {
      typ <- state.types.values
      modelType <- 
        typ match {
          case mtb:ModelTypeBase => Some(mtb)
          case _ => None
        }
    }
      yield modelType
      
    val trivialPath = TrivialPropPath(prop)
    modelPropertiesWithPath(allModelTypes, trivialPath).toSeq :+ trivialPath
  }
}