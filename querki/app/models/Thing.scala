package models

import models.system.SystemSpace
import models.ThingPtr._

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
    val id:ThingPtr, 
    val spaceId:ThingPtr, 
    val model:ThingPtr, 
    val kind:Kind.Kind,
    val props:Map[ThingPtr, PropValue] = Map.empty)
{
  // A couple of convenience methods for the hard-coded Things in System:
  def toProps(pairs:(OID,PropValue)*):Map[ThingPtr, PropValue] = {
    (Map.empty[ThingPtr, PropValue] /: pairs) { (m:Map[ThingPtr, PropValue], pair:(OID, PropValue)) =>
      m + (OID2ThingPtr(pair._1) -> pair._2)
    }
  }
  
  def name(str:String) = (SystemSpace.NameOID -> SystemSpace.TextType(str))
}

/**
 * A ThingState represents the value of a Thing as of a particular time.
 * It is immutable -- you change the Thing by going to its Space and telling it
 * to make the change.
 * 
 * Note that Models are basically just ordinary Things.
 */
case class ThingState(i:ThingPtr, s:ThingPtr, m:ThingPtr) extends Thing(i, s, m, Kind.Thing) {
  
}