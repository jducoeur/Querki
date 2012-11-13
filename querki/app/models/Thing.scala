package models

import play.api._

import models.system.SystemSpace
import models.system.SystemSpace._

/**
 * Enumeration of what sort of Thing this is. Note that this is an intentionally
 * exclusive set. That's mostly to make it reasonably easy to reason about stuff:
 * if something is a Type, that means it isn't ordinary.
 */
object Kind {
  type Kind = Int
  
  val Thing = 0
  val Type = 1
  val Property = 2
  val Space = 3
}

/**
 * The root concept of the entire world. Thing is the Querki equivalent of Object,
 * the basis of the entire type system.
 * 
 * TODO: note that we thread the whole thing with OIDs, to make it easier to build the
 * whole potentially-immutable stack. Down the line, we might add a second pass that
 * re-threads these things with hard references, to make them faster to process. This
 * should do to start, though.
 */
abstract class Thing(
    val id:OID, 
    val spaceId:ThingPtr, 
    val model:ThingPtr, 
    val kind:Kind.Kind) extends ThingPtr
{
  // A couple of convenience methods for the hard-coded Things in System:
  def toProps(pairs:(ThingPtr,PropValue)*):Map[ThingPtr, PropValue] = {
    (Map.empty[ThingPtr, PropValue] /: pairs) { (m:Map[ThingPtr, PropValue], pair:(ThingPtr, PropValue)) =>
      m + (pair._1 -> pair._2)
    }
  }
  
  val props:Map[ThingPtr, PropValue] = Map.empty
  
  def setName(str:String):(OID,PropValue) = (NameOID -> PropValue(str))
  def displayName = getProp(NameProp).render
  
  def space:SpaceState = {
    // TODO: do this for real!
    SystemSpace.State
  }
  
  def getModel:Thing = {
    // TODO: this will eventually need to be able to walk the Space-inheritance tree
    model match {
      case directPtr:Thing => directPtr
      case id:OID => space.thing(id)
    }
  }
  
  /**
   * The Property as defined on *this* specific Thing.
   */
  def localProp(propId:ThingPtr):Option[PropAndVal] = {
    val (pid,ptr) = propId match {
      case i:OID => (i, space.prop(i))
      case t:Thing => (t.id, t.asInstanceOf[Property])
    }
    val byId = props.get(pid)
    if (byId.nonEmpty)
      Some(PropAndVal(ptr, byId.get))
    else
      props.get(ptr).map(v => PropAndVal(ptr, v))
  }
  
  /**
   * The key method for fetching a Property Value from a Thing. This walks the tree
   * as necessary.
   * 
   * Note that this walks up the tree recursively. It eventually ends with UrThing,
   * which does things a little differently.
   */
  def getProp(propId:ThingPtr):PropAndVal = {
    localProp(propId).getOrElse(getModel.getProp(propId))
  }
  
  /**
   * Returns true iff this Thing or any ancestor has the specified property defined on it.
   * Note that this ignores defaults.
   */
  def hasProp(propId:ThingPtr):Boolean = {
    props.contains(propId) || getModel.hasProp(propId)
  }
  
  /**
   * Convenience method -- returns either the value of the specified property or None.
   */
  def getPropOpt(propId:ThingPtr):Option[PropAndVal] = {
    if (hasProp(propId))
      Some(getProp(propId))
    else
      None
  }
  
  // TODO: make the output here into genuine Wikitext
  def renderProps:Wikitext = {
    val listMap = props.map { entry =>
      val prop = space.prop(entry._1)
      "<dt>" + prop.displayName + "</dt><dd>" + prop.render(entry._2) + "</dd>"
    }
    listMap.mkString("<dl>", "", "</dl>")    
  }
  
  /**
   * Every Thing can be rendered -- this returns a Wikitext string that will then be
   * displayed in-page.
   * 
   * TODO: allow this to be redefined with a QL Property if desired.
   */
  def render:Wikitext = {
    val opt = getPropOpt(DisplayTextProp)
    opt.map(pv => TextType.render(pv.v)).getOrElse(renderProps)
  }
}

/**
 * A ThingState represents the value of a Thing as of a particular time.
 * It is immutable -- you change the Thing by going to its Space and telling it
 * to make the change.
 * 
 * Note that Models are basically just ordinary Things.
 */
case class ThingState(i:OID, s:ThingPtr, m:ThingPtr) extends Thing(i, s, m, Kind.Thing) {}