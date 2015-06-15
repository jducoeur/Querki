package models

import scala.annotation.tailrec

import querki.basic.PlainText
import querki.ecology._
import querki.ql.{Invocation, QLFunction, QLPhrase}
import querki.time.DateTime
import querki.util.QLog
import querki.values._

object Thing {
  type PropMap = Map[OID, QValue]
  type PropFetcher = () => PropMap
  
  def emptyProps = Map.empty[OID, QValue]
  
  implicit def bundle2Ops(thing:PropertyBundle)(implicit ecology:Ecology):PropertyBundleOps = thing.thingOps(ecology)
  implicit def thing2Ops(thing:Thing)(implicit ecology:Ecology):ThingOps = thing.thingOps(ecology)
  implicit def space2Ops(state:SpaceState)(implicit ecology:Ecology) = state.spaceStateOps
}

import Thing._

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
    val spaceId:OID, 
    val model:OID, 
    val kind:Kind.Kind,
    val propFetcher: PropFetcher,
    val modTime:DateTime) extends PropertyBundle
{
  lazy val props:PropMap = propFetcher()
  
  /**
   * USE WITH EXTREME CAUTION: this function is for use *only* in the core classes. Its purpose is to allow
   * a Thing to fetch the value of a Property on itself (without inheritance), without using the
   * Ecology. In other words, it does an end-run around ordinary Property fetching and all the
   * type-checking built into that, and instead simply asserts "here's an OID that might exist
   * and might have a single value; if you find it, cast it to this expected type".
   * 
   * (Why bother? It allows the core classes to declare some parameter-free lazy vals.)
   * 
   * If you don't know what you're doing, or don't absolutely need to use this, don't.
   */
  protected def rawLocalProp[T](pid:OID):Option[T] = {
    for {
      qv <- props.get(pid)
      elem <- qv.firstOpt
      raw = elem.elem
    }
      yield raw.asInstanceOf[T]
  }
  
  override def toString = s"$displayName ($id)"
  
  /**
   * The Name of this Thing, if there is one set.
   * 
   * IMPORTANT: only use this if you know what you're doing. Usually, you want displayName instead.
   */
  lazy val linkName:Option[String] = rawLocalProp[String](querki.core.MOIDs.NameOID)
  
  lazy val canonicalName:Option[String] = linkName.filter(_.length() > 0)
  
  lazy val toThingId:ThingId = {
    val nameOpt = canonicalName
    nameOpt map AsName getOrElse AsOID(id)
  }
  
  /**
   * The Display Name of this Thing, rendered as a String.
   * 
   * IMPORTANT: what gets returned from here has already been HTML-processed, and should *not*
   * be re-escaped!
   */
  lazy val displayName:String = displayNameText.toString
  
  def lookupDisplayName:Option[String] = {
    val dispOpt = rawLocalProp[PlainText](querki.basic.MOIDs.DisplayNameOID).map(_.text)
    dispOpt orElse linkName
  }
  
  /**
   * The Display Name of this Thing. This is the underlying form of access, and should
   * be used to get at it as Html or HtmlWikitext. It has already been HTML-neutered, and
   * is the safest and most flexible way to use this name.
   */
  lazy val displayNameText:DisplayText = {
    val display = for {
      localName <- lookupDisplayName
      rendered = Wikitext(localName).raw
      if (rendered.str.length() > 0)
    }
      yield rendered
      
    display.getOrElse(DisplayText(id.toThingId.toString))
  }
  
  /**
   * The *literal* Display Name of this Thing, exactly as typed.
   * 
   * IMPORTANT: this value has *NOT* been HTML-escaped. It must only be used in an environment that
   * will do the escaping sometime later! Do not use this casually -- always test the environment that
   * you will be using it in!
   */
  lazy val unsafeDisplayName:String = {
    val display = for {
      localName <- lookupDisplayName
      rendered = Wikitext(localName).plaintext
      if (rendered.length() > 0)
    }
      yield rendered
      
    display.getOrElse(id.toThingId.toString)
  }
  
  def isThing:Boolean = true
  def asThing:Option[Thing] = Some(this)

  /**
   * DEPRECATED: use getModelOpt instead! It is not only more correct, it's likely to be faster!
   */
  def getModel(implicit state:SpaceState):Thing = { 
    state.anything(model).getOrElse{
      try {
        // TODO: make this work again!
        throw new Exception("Trying to get unknown Model " + model + " for "/* + displayName*/)
      } catch {
        case error:Exception => QLog.error("Unable to find Model", error); throw error
      }
    }
  }
  def getModelOpt(implicit state:SpaceState):Option[Thing] = {
    if (hasModel) Some(getModel) else None
  }
  def hasModel = (model != UnknownOID)
  
  /**
   * The Property as defined on *this* specific Thing.
   */
  def localProp(pid:OID)(implicit state:SpaceState):Option[PropAndVal[_]] = {
    val propOpt = state.prop(pid)
    propOpt.flatMap(prop => props.get(pid).map(v => prop.pair(v)))
  }
  // Note that this does *not* require a SpaceState. This is very, very important, so that it can be used
  // for lazy vals:
  def localProp[VT, CT](prop:Property[VT, _]):Option[PropAndVal[VT]] = {
    prop.fromOpt(this.props) map prop.pair
  }
  
  /**
   * The key method for fetching a Property Value from a Thing. This walks the tree
   * as necessary.
   * 
   * Note that this walks up the tree recursively. It eventually ends with UrThing,
   * which does things a little differently.
   */
  def getProp(propId:OID)(implicit state:SpaceState):PropAndVal[_] = {
    // TODO: we're doing redundant lookups of the property. Rationalize this stack of calls.
    val propOpt = state.prop(propId)
    propOpt match {
      case Some(prop) => 
        if (prop.notInherited)
          localOrDefault(propId)
        else
          localProp(propId).getOrElse(getModelOpt.map(_.getProp(propId)).getOrElse(prop.defaultPair))
      case None => throw new Exception("Trying to look up unknown Property " + propId)
    }
  }
  def getProp[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropAndVal[VT] = {
    // TODO: we're doing redundant lookups of the property. Rationalize this stack of calls.
    // Note: the != here is because NotInheritedProp gets into an infinite loop otherwise:
    if (this.id != querki.core.MOIDs.NotInheritedOID && prop.notInherited)
      localOrDefault(prop)
    else
      localProp(prop).getOrElse(getModelOpt.map(_.getProp(prop)).getOrElse(prop.defaultPair))
  }
  
  def localOrDefault(propId:OID)(implicit state:SpaceState):PropAndVal[_] = {
    val prop = state.prop(propId).getOrElse(throw new Exception("Using localOrDefault on an unknown Property!"))
    localProp(propId).getOrElse(prop.defaultPair)
  }
  def localOrDefault[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropAndVal[VT] = {
    localProp(prop).getOrElse(prop.defaultPair)
  }
    
  /**
   * If you have the actual Property object you're looking for, this returns its value
   * on this object in a typesafe way.
   */
  def getPropVal[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):QValue = {
    val local = localPropVal(prop)
    if (local.isDefined)
      local.get
    else if (prop.notInherited)
      prop.default
    else
      getModel.getPropVal(prop)
  }

  /**
   * Return the first value in the collection for this Type. This is a convenient, type-safe way to
   * get at a property value for ExactlyOne properties.
   * 
   * IMPORTANT: this will throw an Exception if you try to call it on an Optional that is empty!
   * In general, while it is syntactically legal to call this on an Optional type, it's usually
   * inappropriate.
   * 
   * DEPRECATED: you should use firstOpt instead!
   */
  def first[VT](prop:Property[VT, _])(implicit state:SpaceState):VT = {
    getProp(prop).first
  }
  
  def localFirst[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[VT] = {
    localProp(prop) map (_.first)
  }
  
  /**
   * The good, safe way to retrieve the value of a Property, and transform it into something else.
   * Will *always* return a QValue, which will be empty iff the property isn't defined or the value
   * is empty.
   */
  def map[VT, DT, RT](prop:Property[VT, _], destType:PType[DT] with PTypeBuilder[DT, RT])(cb:VT => RT)(implicit state:SpaceState):QValue = {
//    val propAndValOpt = getPropOpt(prop)
//    propAndValOpt match {
//      case Some(propAndVal) => propAndVal.map(destType)(cb)
//      case None => Optional.Empty(destType)
//    }
    // TBD: does this do the desired thing? It is strictly correct, but if the Property is
    // ExactlyOne, it will *always* return a value.
    val propAndVal = getProp(prop)
    propAndVal.map(destType)(cb)
  }
  
  /**
   * Returns the first value of the specified Property *or* a given default.
   */
  def firstOr[VT](prop:Property[VT, _], default:VT)(implicit state:SpaceState):VT = {
    val cv = getProp(prop)
    if (cv.isEmpty)
      default
    else
      cv.first
  }
  
  /**
   * Returns the first value of the specified Property *or* None.
   */
  def firstOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[VT] = {
    val cv = getProp(prop)
    if (cv.isEmpty)
      None
    else
      Some(cv.first)
  }
  
  /**
   * Convenience method -- returns either the value of the specified property or None.
   */
  def getPropOpt(propId:OID)(implicit state:SpaceState):Option[PropAndVal[_]] = {
    state.prop(propId).flatMap { prop =>
      if (prop.notInherited)
        localProp(prop)
      else
        getUntypedPropOptRec(prop)
    }
  }
  // TODO: fix the duplication of these two methods. It appears that the compiler occasionally is choosing
  // the less-powerful OID version of getPropOpt instead of the one I want, presumably through the implicit
  // conversion of Property to OID.
  def getPropOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]] = {
    getPropOptTyped(prop)
  }
  def getPropOptTyped[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]] = {
    if (prop.notInherited)
      localProp(prop)
    else
      getPropOptRec(prop)
  }
  
  // TBD: is it possible to combine these? The Untyped version seems to be necessary to make 
  // getPropOpt(OID) work, but it really feels like there should be a way to work around it...
  private def getPropOptRec[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]] = {
    localProp(prop).orElse(getModelOpt.flatMap(_.getPropOptRec(prop)))
  }
  private def getUntypedPropOptRec(prop:Property[_, _])(implicit state:SpaceState):Option[PropAndVal[_]] = {
    localProp(prop).orElse(getModelOpt.flatMap(_.getUntypedPropOptRec(prop)))
  }
  
  def thingOps(e:Ecology) = new ThingOps(this)(e)
}

/**
 * A ThingState represents the value of a Thing as of a particular time.
 * It is immutable -- you change the Thing by going to its Space and telling it
 * to make the change.
 * 
 * Note that Models are basically just ordinary Things.
 */
case class ThingState(i:OID, s:OID, m:OID, pf: PropFetcher, mt:DateTime = querki.time.epoch, k:Kind.Kind = Kind.Thing)
  extends Thing(i, s, m, k, pf, mt) {}