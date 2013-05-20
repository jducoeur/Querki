package models

import play.api._
import play.api.templates.Html

import models.system._
import models.system.OIDs._
import models.system.SystemSpace
import models.system.SystemSpace._

import NameCollection.bootProp

import controllers.RequestContext

import ql._

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
  val Collection = 4
  val Attachment = 5
}

object Thing {
  type PropMap = Map[OID, PropValue]
  type PropFetcher = () => PropMap
  
  // A couple of convenience methods for the hard-coded Things in System:
  def toProps(pairs:(OID,PropValue)*):PropFetcher = () => {
    (Map.empty[OID, PropValue] /: pairs) { (m:Map[OID, PropValue], pair:(OID, PropValue)) =>
      m + (pair._1 -> pair._2)
    }
  }
  
  def emptyProps = Map.empty[OID, PropValue]
  
  // NOTE: don't try to make this more concise -- it causes chicken-and-egg problems in system
  // initialization:
  def setName(str:String):(OID,PropValue) = bootProp(NameOID, str)
//    (NameOID -> ExactlyOne(ElemValue(str)))
//    (NameOID -> PropValue(Some(ElemValue(str))))

  // TODO: this escape/unescape is certainly too simplistic to cope with recursive types.
  // Come back to this sometime before we make the type system more robust.
  def escape(str:String) = {
    str.replace("\\", "\\\\").replace(";", "\\;").replace(":", "\\:").replace("}", "\\}").replace("{", "\\{")
  }
  def unescape(str:String) = {
    str.replace("\\{", "{").replace("\\}", "}").replace("\\:", ":").replace("\\;", ";").replace("\\\\", "\\")
  }
  
  def serializeProps(props:PropMap, space:SpaceState) = {
    val serializedProps = props.map { pair =>
      val (ptr, v) = pair
      val prop = space.prop(ptr)
      val oid = prop.id
      oid.toString + 
        ":" + 
        Thing.escape(prop.serialize(prop.castVal(v)))
    }
    
    serializedProps.mkString("{", ";", "}")
  }    
  
  def deserializeProps(str:String, space:SpaceState):PropMap = {
    // Strip the surrounding {} pair:
    val stripExt = str.slice(1, str.length() - 1)
    // Note that we have to split on semicolons that are *not* preceded by backslashes. This is
    // a little tricky to express in regex -- the weird bit is saying "things that aren't backslashes,
    // non-capturing".
    val propStrs = stripExt.split("""(?<=[^\\]);""")
    val propPairs = propStrs.filter(_.trim.length() > 0).map { propStr =>
      val (idStr, valStrAndColon) = propStr.splitAt(propStr.indexOf(':'))
      val valStr = unescape(valStrAndColon.drop(1))
      val id = OID(idStr)
      val prop = space.prop(id)
      val v = prop.deserialize(valStr)
      (id, v)
    }
    toProps(propPairs:_*)()
  }
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
    val propFetcher: PropFetcher) extends QLFunction
{
  lazy val props:PropMap = propFetcher()
  
  def thisAsContext(implicit request:RequestContext) = QLContext(TypedValue(ExactlyOne(LinkType(this.id)), LinkType), request)
  
  def displayName:String = {
    val localName = localProp(DisplayNameProp) orElse localProp(NameProp)
    if (localName.isEmpty)
      id.toString
    else {
      localName.get.renderPlain.raw
    }
  }
  
  def canonicalName:Option[String] = {
    NameProp.firstOpt(props)
  }
  
  def toThingId:ThingId = {
    val nameOpt = canonicalName
    nameOpt map AsName getOrElse AsOID(id)
  }

  def getModel(implicit state:SpaceState):Thing = { 
    state.anything(model).getOrElse{ Logger.error("Unable to find Model for " + id); UrThing } 
  }
  def hasModel = true
  
  /**
   * The Property as defined on *this* specific Thing.
   */
  def localProp(pid:OID)(implicit state:SpaceState):Option[PropAndVal[_]] = {
    val ptr = state.prop(pid)
    props.get(pid).map(v => ptr.pair(v))
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
    val prop = state.prop(propId)
    if (prop.first(NotInheritedProp))
      localOrDefault(propId)
    else
      localProp(propId).getOrElse(getModel.getProp(propId))
  }
  def getProp[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropAndVal[VT] = {
    // TODO: we're doing redundant lookups of the property. Rationalize this stack of calls.
    if (prop.first(NotInheritedProp))
      localOrDefault(prop)
    else
      localProp(prop).getOrElse(getModel.getProp(prop))
  }
  
  def localPropVal[VT, CT](prop:Property[VT, _]):Option[PropValue] = {
    prop.fromOpt(props)
  }
  
  def localOrDefault(propId:OID)(implicit state:SpaceState):PropAndVal[_] = {
    val prop = state.prop(propId)
    localProp(propId).getOrElse(prop.defaultPair)
  }
  def localOrDefault[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropAndVal[VT] = {
    localProp(prop).getOrElse(prop.defaultPair)
  }
    
  /**
   * If you have the actual Property object you're looking for, this returns its value
   * on this object in a typesafe way.
   */
  def getPropVal[VT, CT](prop:Property[VT, _])(implicit state:SpaceState):PropValue = {
    val local = localPropVal(prop)
    if (local.isDefined)
      local.get
    else if (prop.first(NotInheritedProp))
      prop.default
    else
      getModel.getPropVal(prop)
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
   * Return the first value in the collection for this Type. This is a convenient, type-safe way to
   * get at a property value for ExactlyOne properties.
   * 
   * IMPORTANT: this will throw an Exception if you try to call it on an Optional that is empty!
   * In general, while it is syntactically legal to call this on an Optional type, it's usually
   * inappropriate.
   */
  def first[VT](prop:Property[VT, _])(implicit state:SpaceState):VT = {
    prop.first(getPropVal(prop))
  }
  
  def localFirst[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[VT] = {
    localPropVal(prop) map (prop.first(_))
  }
  
  /**
   * Returns the first value of the specified Property *or* a given default.
   */
  def firstOr[VT](prop:Property[VT, _], default:VT)(implicit state:SpaceState):VT = {
    val cv = getPropVal(prop)
    if (prop.isEmpty(cv))
      default
    else
      prop.first(cv)  
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
    ifSet(IsModelProp)
  }
  
  /**
   * Returns true iff this Thing or any ancestor has the specified property defined on it.
   * Note that this ignores defaults.
   */
  def hasProp(propId:OID)(implicit state:SpaceState):Boolean = {
    props.contains(propId) || getModel.hasProp(propId)
  }
  
  /**
   * Convenience method -- returns either the value of the specified property or None.
   */
  def getPropOpt(propId:OID)(implicit state:SpaceState):Option[PropAndVal[_]] = {
    if (hasProp(propId))
      Some(getProp(propId))
    else
      None
  }
  def getPropOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]] = {
    if (hasProp(prop))
      Some(getProp(prop))
    else
      None
  }
  
  def localProps(implicit state:SpaceState):Set[Property[_,_]] = {
    props.keys.map(state.prop(_)).toSet    
  }
  
  /**
   * Lists all of the Properties defined on this Thing and its ancestors.
   */
  def allProps(implicit state:SpaceState):Set[Property[_,_]] = {
    localProps ++ getModel.allProps
  }
  
  /**
   * True iff the other is an ancestor of this Thing via the Model chain.
   */
  def isAncestor(other:OID)(implicit state:SpaceState):Boolean = {
    (other == model) || getModel.isAncestor(other)
  }
  
  def renderProps(implicit request:RequestContext):Wikitext = {
    val listMap = props.map { entry =>
      val prop = request.state.get.prop(entry._1)
      val pv = prop.pair(entry._2)
      "<dt>" + prop.displayName + "</dt><dd>" + pv.render(thisAsContext).raw + "</dd>"
    }
    HtmlWikitext(Html(listMap.mkString("<dl>", "", "</dl>")))    
  }
  
  /**
   * Every Thing can be rendered -- this returns a Wikitext string that will then be
   * displayed in-page.
   * 
   * If you specify a property, that property will be rendered with this Thing as a context;
   * otherwise, DisplayText will be rendered.
   * 
   * TODO: allow this to be redefined with a QL Property if desired.
   */
  def render(implicit request:RequestContext, prop:Option[Property[_,_]] = None):Wikitext = {
    implicit val state = request.state.get
    val opt = getPropOpt(prop.getOrElse(DisplayTextProp))
    opt.map(pv => pv.render(thisAsContext)).getOrElse(renderProps)
  }
  
  /**
   * Called when this Thing is encountered with no method invocation in a QL expression.
   * Subclasses are allowed to override it as make sense.
   * 
   * This basic version returns a Link to this thing.
   * TODO: add a "self" method to always be able to do this.
   * 
   * TODO: add a Property to allow runtime Things to override this.
   */
  def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
    TypedValue(ExactlyOne(LinkType(id)), LinkType)
  }  
  
  /**
   * This is specifically for the right-hand side of a dot in QL processing. This counts
   * as partial application, and should return a function that will handle the rest of the
   * function.
   * 
   * Partial application is nonsensical for most Things; it is mainly intended for methods
   * on properties.
   */
  def partiallyApply(context:ContextBase):QLFunction = {
    new BogusFunction
  }

  def serializeProps(implicit state:SpaceState) = Thing.serializeProps(props, state)
  
  def export(implicit state:SpaceState):String = {
    "{" +
    "id:" + id.toString + ";" +
    "model:" + model.id.toString + ";" +
    "kind:" + kind.toString + ";" +
    "props:" + serializeProps +
    "}"
  }
}

/**
 * A ThingState represents the value of a Thing as of a particular time.
 * It is immutable -- you change the Thing by going to its Space and telling it
 * to make the change.
 * 
 * Note that Models are basically just ordinary Things.
 */
case class ThingState(i:OID, s:OID, m:OID, pf: PropFetcher, k:Kind.Kind = Kind.Thing) 
  extends Thing(i, s, m, k, pf) {}