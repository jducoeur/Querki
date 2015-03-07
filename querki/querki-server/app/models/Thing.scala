package models

import scala.annotation.tailrec

import querki.ecology._
import querki.ql.{Invocation, QLFunction, QLPhrase}
import querki.time.DateTime
import querki.util.QLog
import querki.values._

object Thing {
  type PropMap = Map[OID, QValue]
  type PropFetcher = () => PropMap
  
  def emptyProps = Map.empty[OID, QValue]
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
    val modTime:DateTime)(implicit val ecology:Ecology) extends QLFunction with PropertyBundle with EcologyMember
{
  lazy val props:PropMap = propFetcher()
  
  def thisAsQValue:QValue = Core.ExactlyOne(Core.LinkType(this.id))
  
  // These are defs instead of vals, because any vals defined here will be for every single Thing in the
  // world. Don't val-ify too casually. In this case, I believe we're willing to accept a little lookup
  // overhead, to save space:
  def DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  def Core = interface[querki.core.Core]
  def Basic = interface[querki.basic.Basic]
  def QL = interface[querki.ql.QL]
  
  def ApplyMethod = Basic.ApplyMethod
  def NotInheritedProp = Core.NotInheritedProp
  def NameProp = Core.NameProp
  
  override def toString = s"$displayName ($id)"
  
  /**
   * The Name of this Thing, if there is one set.
   * 
   * IMPORTANT: only use this if you know what you're doing. Usually, you want displayName instead.
   */
  lazy val linkName:Option[String] = {
    for (
      nameVal <- localProp(NameProp);
      plaintext <- nameVal.firstOpt 
        )
      yield plaintext
  }
  
  /**
   * The Display Name of this Thing, rendered as a String.
   * 
   * IMPORTANT: what gets returned from here has already been HTML-processed, and should *not*
   * be re-escaped!
   */
  lazy val displayName:String = displayNameText.toString
  
  def lookupDisplayName:Option[PropAndVal[_]] = {
    val dispOpt = localProp(DisplayNameProp)
    if (dispOpt.isEmpty || dispOpt.get.isEmpty)
      localProp(NameProp)
    else
      dispOpt
  }
  
  /**
   * The Display Name of this Thing. This is the underlying form of access, and should
   * be used to get at it as Html or HtmlWikitext. It has already been HTML-neutered, and
   * is the safest and most flexible way to use this name.
   */
  lazy val displayNameText:DisplayText = {
    val localName = lookupDisplayName
    if (localName.isEmpty)
      DisplayText(id.toThingId.toString)
    else {
      val rendered = localName.get.renderPlain.raw
      if (rendered.str.length() > 0)
        rendered
      else
        DisplayText(id.toThingId.toString)
    }    
  }
  
  /**
   * The *literal* Display Name of this Thing, exactly as typed.
   * 
   * IMPORTANT: this value has *NOT* been HTML-escaped. It must only be used in an environment that
   * will do the escaping sometime later! Do not use this casually -- always test the environment that
   * you will be using it in!
   */
  lazy val unsafeDisplayName:String = {
    val localName = lookupDisplayName
    if (localName.isEmpty)
      id.toString
    else {
      val rendered = localName.get.renderPlain.plaintext
      if (rendered.length() > 0)
        rendered
      else
        id.toString
    }    
  }
  
  lazy val canonicalName:Option[String] = {
    NameProp.firstOpt(props).filter(_.length() > 0)
  }
  
  lazy val toThingId:ThingId = {
    val nameOpt = canonicalName
    nameOpt map AsName getOrElse AsOID(id)
  }
  
  def isThing:Boolean = true
  def asThing:Option[Thing] = Some(this)

  /**
   * DEPRECATED: use getModelOpt instead! It is not only more correct, it's likely to be faster!
   */
  def getModel(implicit state:SpaceState):Thing = { 
    state.anything(model).getOrElse{
      try {
        throw new Exception("Trying to get unknown Model " + model + " for " + displayName)
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
        if (prop.first(NotInheritedProp))
          localOrDefault(propId)
        else
          localProp(propId).getOrElse(getModelOpt.map(_.getProp(propId)).getOrElse(prop.defaultPair))
      case None => throw new Exception("Trying to look up unknown Property " + propId)
    }
  }
  def getProp[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropAndVal[VT] = {
    // TODO: we're doing redundant lookups of the property. Rationalize this stack of calls.
    // Note: the != here is because NotInheritedProp gets into an infinite loop otherwise:
    if (this != NotInheritedProp && prop.first(NotInheritedProp))
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
    else if (prop.first(NotInheritedProp))
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
   * Convenience method, to check whether a YesNo Property is non-empty, and is true.
   */
  def ifSet(prop:Property[Boolean, _])(implicit state:SpaceState):Boolean = {
    firstOr(prop, false)
  }
  
  /**
   * Returns true iff this Thing has the IsModel flag set to true on it.
   */
  def isModel(implicit state:SpaceState):Boolean = {
    ifSet(Core.IsModelProp)
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
    
  def localPropsAndVals(implicit state:SpaceState):Iterable[PropAndVal[_]] = {
    for (
      entry <- props;
      prop <- state.prop(entry._1)
        )
      yield prop.pair(entry._2)
  }
  
  /**
   * True iff the other is an ancestor of this Thing via the Model chain.
   */
  def isAncestor(other:OID)(implicit state:SpaceState):Boolean = {
    (other == model) || getModelOpt.map(_.isAncestor(other)).getOrElse(false)
  }
  
  def renderProps(implicit request:RequestContext):Wikitext = {
    request.renderer.renderThingDefault(this)
  }
  
  /**
   * Show the default rendering for this Thing, if it has no DisplayTextProp defined.
   * 
   * This mainly exists so that the different Kinds can override it and do their own thing.
   */
  def renderDefault(implicit request:RequestContext):Wikitext = {
    renderProps(request)
  }
  
  /**
   * Every Thing can be rendered -- this returns a Wikitext string that will then be
   * displayed in-page.
   * 
   * If you specify a property, that property will be rendered with this Thing as a context;
   * otherwise, DisplayText will be rendered.
   * 
   * TODO: allow this to be redefined with a QL Property if desired.
   * TODO: the prop-rendering version of this is just plain broken. Deprecate the second
   * parameter, and probably just do that by hand in the couple of places it's needed.
   */
  def render(implicit request:RequestContext, prop:Option[Property[_,_]] = None):Wikitext = {
    implicit val state = request.state.get
    val actualProp = 
      if (ifSet(Core.IsModelProp))
        prop.getOrElse(Basic.ModelViewProp)
      else
        prop.getOrElse(Basic.DisplayTextProp)
    val renderedOpt = for (
      pv <- getPropOpt(actualProp);
      if (!pv.isEmpty)
        )
      yield pv.render(thisAsContext.forProperty(pv.prop), Some(this))
    
    renderedOpt.getOrElse(renderDefault)
  }
  
  /**
   * Called when this Thing is encountered with no method invocation in a QL expression.
   * Subclasses are allowed to override it as make sense.
   * 
   * This basic version returns a Link to this thing.
   */
  def qlApply(inv:Invocation):QValue = {
    val context = inv.context
    val paramsOpt = inv.paramsOpt
    
    val applyOpt = getPropOpt(ApplyMethod)(context.state)
    applyOpt match {
      case Some(apply) => {
        val qlText = apply.first
        QL.processMethod(qlText, context.forProperty(apply.prop), Some(inv), Some(this))
      }
      case None => Core.ExactlyOne(Core.LinkType(id))
    }
  }
  
  class BogusFunction extends QLFunction {
    def qlApply(inv:Invocation):QValue = {
      QL.WarningValue("It does not make sense to put this after a dot.")
    }
  }

  /**
   * This is specifically for the right-hand side of a dot in QL processing. This counts
   * as partial application, and should return a function that will handle the rest of the
   * function.
   * 
   * Partial application is nonsensical for most Things; it is mainly intended for methods
   * on properties.
   */
  def partiallyApply(context:QLContext):QLFunction = {
    new BogusFunction
  }
}

/**
 * A ThingState represents the value of a Thing as of a particular time.
 * It is immutable -- you change the Thing by going to its Space and telling it
 * to make the change.
 * 
 * Note that Models are basically just ordinary Things.
 */
case class ThingState(i:OID, s:OID, m:OID, pf: PropFetcher, mt:DateTime = querki.time.epoch, k:Kind.Kind = Kind.Thing)(implicit e:Ecology) 
  extends Thing(i, s, m, k, pf, mt)(e) with EcologyMember {}