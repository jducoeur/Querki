package models

import models.Thing.{PropMap, emptyProps}

import querki.globals._
import querki.values.{PropAndVal, QLContext, QValue, RequestContext, SpaceState}

/**
 * PropertyBundle is the abstraction of "a bunch of Properties", from which you can fetch a specific Property value.
 */
trait PropertyBundle {
  /**
   * Returns true iff this bundle is an actual Thing, that you can treat on its own for operations like
   * deleting or finding a single identity.
   */
  def isThing:Boolean
  
  /**
   * Iff this is an actual Thing, return that; otherwise, None. Intended to use with match.
   */
  def asThing:Option[Thing]
  
  /**
   * Does this Thing have a Model? Almost (but not quite) always true.
   */
  def hasModel:Boolean
  
  /**
   * If this Thing has a Model, returns it; otherwise, throw an Exception.
   * 
   * DEPRECATED: prefer getModelOpt instead!
   */
  def getModel(implicit state:SpaceState):Thing
  
  /**
   * Iff this Thing has a Model, returns it.
   */
  def getModelOpt(implicit state:SpaceState):Option[Thing]
  
  /**
   * Fetch the actual Property values contained in this Bundle.
   */
  def props:PropMap
  
  def localProps(implicit state:SpaceState):Set[Property[_,_]] = {
    val propOpts = props.keys.map(state.prop(_))
    val validProps = propOpts.flatten
    validProps.toSet    
  }
  
  def localPropVal[VT, CT](prop:Property[VT, _]):Option[QValue] = {
    prop.fromOpt(props)
  }
  
  def getDisplayPropVal[VT, _](prop:Property[VT, _])(implicit state:SpaceState):DisplayPropVal = {
    val local = localPropVal(prop)
    local match {
      case Some(v) => DisplayPropVal(Some(this), prop, Some(v))
      case None => {
        val inheritedVal = getPropOpt(prop)
        DisplayPropVal(Some(this), prop, None, inheritedVal.map(_.v))
      }
    }
  }
  
  /**
   * Lists all of the Properties defined on this Thing and its ancestors.
   */
  def allProps(implicit state:SpaceState):Set[Property[_,_]] = {
    val ancestorProps:Set[Property[_,_]] = getModelOpt.map(_.allProps).getOrElse(Set.empty) 
    localProps ++ ancestorProps
  }
  
  /**
   * Fetches the values of this Property as defined on this Thing, *not* on its Model.
   */
  def localProp[VT, CT](prop:Property[VT, _]):Option[PropAndVal[VT]]
  
  /**
   * If you have the actual Property object you're looking for, this returns its value
   * on this object in a typesafe way.
   */
  def getPropVal[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):QValue
  
  /**
   * Returns true iff this Thing or any ancestor has the specified property defined on it.
   * Note that this ignores defaults.
   */
  def hasProp(propId:OID)(implicit state:SpaceState):Boolean = {
    props.contains(propId) || getModelOpt.map(_.hasProp(propId)).getOrElse(false)
  }
  
  /**
   * The good, safe way to retrieve the value of a Property, and transform it into something else.
   * Will *always* return a QValue, which will be empty iff the property isn't defined or the value
   * is empty.
   */
  def map[VT, DT, RT](prop:Property[VT, _], destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT)(implicit state:SpaceState):QValue
  
  /**
   * Fetches the values of the given Property on this Thing, or None if it isn't defined.
   */
  def getPropOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]]

  /**
   * Allows you to iterate over the values of this Property on this Bundle, if there are any.
   * 
   * If this Bundle doesn't define this Property, this will silently produce an empty List. Note
   * that there is no way, using this function, to distinguish between the Property not being set
   * at all, or being set to an empty Optional, QList or QSet!
   * 
   * This is simply a convenience function, but a common enough use case to be useful.
   */
  def getPropAll[VT](prop:Property[VT, _])(implicit state:SpaceState):List[VT] = {
    getPropOpt(prop) match {
      case Some(pv) => pv.rawList
      case None => List.empty[VT]
    }
  }
  
  /**
   * Fetches the first value of the given Property on this Thing, if there is one.
   * 
   * Only use this if you are confident that it can have at most one value!
   * 
   * TODO: I believe this renders Thing.firstOpt() redundant, but look at that carefully -- they aren't
   * identical, by any means.
   */
  def getFirstOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[VT] = {
    getPropOpt(prop).flatMap(_.firstOpt)
  }
  
  def thingOps(e:Ecology):PropertyBundleOps
}

abstract class PropertyBundleOps(bundle:PropertyBundle)(implicit val ecology:Ecology) extends EcologyMember {
  
  /**
   * Wraps this Bundle in a QValue.
   */
  def thisAsQValue:QValue
  
  /**
   * Given the request we're operating within, this produces a Context you can use for
   * handle QL expressions.
   */
  def thisAsContext(implicit request:RequestContext, state:SpaceState, ecology:Ecology):QLContext = QLContext(thisAsQValue, Some(request))
  
}