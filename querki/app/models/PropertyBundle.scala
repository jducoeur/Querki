package models

import models.Thing.{PropMap, emptyProps}

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
   * Given the request we're operating within, this produces a Context you can use for
   * handle QL expressions.
   */
  def thisAsContext(implicit request:RequestContext):QLContext
  
  /**
   * Fetch the actual Property values contained in this Bundle.
   */
  def props:PropMap
  
  def localProps(implicit state:SpaceState):Set[Property[_,_]] = {
    val propOpts = props.keys.map(state.prop(_))
    val validProps = propOpts.flatten
    validProps.toSet    
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
   * The good, safe way to retrieve the value of a Property, and transform it into something else.
   * Will *always* return a QValue, which will be empty iff the property isn't defined or the value
   * is empty.
   */
  def map[VT, DT, RT](prop:Property[VT, _], destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT)(implicit state:SpaceState):QValue
  
  /**
   * Fetches the values of the given Property on this Thing, or None if it isn't defined.
   */
  def getPropOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]]
}
