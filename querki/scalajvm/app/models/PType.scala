package models

import scala.concurrent.Future
import scala.xml.NodeSeq

import Thing.{PropFetcher, emptyProps}

import querki.ecology.Ecology
import querki.ql.Invocation
import querki.util.QLog
import querki.values._

/**
 * Properties have Types. There's nothing controversial here -- Types are usually
 * things like Text, Number and so on. But note that Types are themselves Things;
 * this is specifically so that we can potentially add user-defined Types down
 * the road.
 */
abstract class PType[VT](i:OID, s:OID, m:OID, pf:PropFetcher) extends Thing(i, s, m, Kind.Type, pf, querki.time.epoch) {
  
  type valType = VT

  /**
   * Each PType is required to implement this -- it is the deserializer for the
   * type.
   */
  def doDeserialize(ser:String)(implicit state:SpaceState):VT
  final def deserialize(ser:String)(implicit state:SpaceState):ElemValue = 
    try {
      ElemValue(doDeserialize(ser), this)
    } catch {
      case error:Exception => {
        QLog.error(s"Type ${displayName}: error trying to deserialize string '$ser'", error)
        // Give up and use the default value. This is specifically an error-stop, so that corruptions in one
        // property value don't prevent the entire Thing from loading:
        default
      }
    }
  
  /**
   * Also required for all PTypes, to serialize values of this type.
   */
  def doSerialize(v:VT)(implicit state:SpaceState):String
  final def serialize(v:ElemValue)(implicit state:SpaceState):String = doSerialize(get(v))
  
  /**
   * Takes a value of this type, and turns it into displayable form. Querki
   * equivalent to toString.
   */
  def doWikify(context:QLContext)(v:VT, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext]
  /**
   * Take a value of this type and turn it into a Wikitext. Formerly called "render", but that conflicts
   * in weird signature ways with Thing.render. (It appears that you can't have multiple overloads with
   * default values, even if the previous parameters differentiate between the overloads.)
   */
  final def wikify(context:QLContext)(v:ElemValue, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = 
    doWikify(context)(get(v), displayOpt, lexicalThing)
    
  /**
   * This gives a PType the opportunity to take complete control of the rendering of a QValue of this PType,
   * without going through the Collection. In this case, doWikify() will never be called from the outside;
   * the PType needs to do everything. This is an exceptional case, and only for use when, eg, a PType needs to
   * handle List display differently than the usual.
   * 
   * TBD: this is a bit of a hack, originally intended as a way to inject a little "wrapper" for Photo Lists. Would it be
   * better to *always* put a wrapper around Lists and Sets? That would break a thousand unit tests, and we would
   * need to figure out whether that wrapper is a div or span, but it seems conceptually plausible.
   */
  def fullWikify(context:QLContext, qv:QValue, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Option[Future[Wikitext]] = None
  
  /**
   * Takes a value of this type, and renders it for showing in debug messages.
   */
  def doDebugRender(context:QLContext)(v:VT) = v.toString
  final def debugRender(context:QLContext)(v:ElemValue):String = doDebugRender(context)(get(v))
  
  /**
   * Also required for all PTypes -- the default value to fall back on.
   */
  def doDefault(implicit state:SpaceState):VT
  final def default(implicit state:SpaceState):ElemValue = ElemValue(doDefault, this)
  
  /**
   * Turns this value into an appropriate form for user editing. Currently that means
   * a String, although that's likely to get more interesting soon.
   */
  def doToUser(v:VT)(implicit state:SpaceState):String = doSerialize(v)
  final def toUser(v:ElemValue)(implicit state:SpaceState):String = doToUser(get(v))
  
  /**
   * Turns this value into a format appropriate for a URL parameter.
   */
  def doToUrlParam(v:VT, raw:Boolean)(implicit state:SpaceState):String = doToUser(v)
  final def toUrlParam(v:ElemValue, raw:Boolean)(implicit state:SpaceState):String = doToUrlParam(get(v), raw)
  
  /**
   * This compares two values. It is used to sort Collections. It should return true iff left is "less than" right.
   */
  def doComp(context:QLContext)(left:VT, right:VT):Boolean = {
    implicit val s = context.state
    math.Ordering.String.lt(doToUser(left), doToUser(right)) 
  } 
  final def comp(context:QLContext)(left:ElemValue, right:ElemValue):Boolean = doComp(context)(get(left), get(right))
  
  /**
   * The type unwrapper -- takes an opaque ElemValue and returns the underlying value.
   * This is a fundamentally unsafe operation, so it should always be performed in the
   * context of a Property.
   */
  def get(v:ElemValue):VT = v.get(this)
  
  /**
   * Can this String value be legitimately interpreted as this type? This either passes, or
   * throws an Exception, preferably a useful PublicException.
   * 
   * This is closely related to doFromUser -- iff something can be parsed by doFromUser, it
   * should validate cleanly. It is intended for UI use.
   * 
   * Types may override this, especially if their validation is related to the Property.
   * (Typically because they interact with other meta-Properties on this one.)
   * 
   * IMPORTANT: this can throw Exceptions, and specifically PublicExceptions! Calls must be
   * wrapped in a Tryer!
   */
  def validate(v:String, prop:Property[_,_], state:SpaceState):Unit = {
    doFromUser(v)(state)
  }
  
  /**
   * Display the appropriate input control for this type, with the default value set as specified.
   * 
   * All Types that can be user-input should define this.
   * 
   * TBD: in practice, I don't love this. The coupling is roughly correct, but it winds up mixing
   * HTML-specific code into a very primitive level of the system. There should probably instead be
   * side classes for each PType, which describe how to render them in particular circumstances. But
   * we'll get to that...
   */
  def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq
  def renderInput(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
    renderInputXml(prop, context, currentValue, v)
  }
  
  /**
   * Parses form input from the user. By default, we assume that this is the same
   * as deserialization, but override this when that's not true.
   * 
   * This should throw an Exception if the input is not legal. This is used in
   * validation.
   */
  protected def doFromUser(str:String)(implicit state:SpaceState):VT = doDeserialize(str)
  final def fromUser(str:String)(implicit state:SpaceState):ElemValue = ElemValue(doFromUser(str), this)
  
  /**
   * If this Type implies special processing when named in a QL expression (other than simply returning
   * the value of the property), override this method
   * The guts of applying a QL function. Note that this allows two contexts, which is often relevant
   * for these:
   * 
   *   incomingContext -> definingContext.prop(params)
   *   
   * If this isn't partially applied, the incomingContext is used for both. See Property for the main
   * usage of this.
   */
  def qlApplyFromProp(inv:Invocation, prop:Property[VT,_]):Option[QFut] = None
  
  /**
   * Iff defined, this Type must *always* be used with the specified Collection.
   * 
   * This is mostly intended for use with Type Aliases.
   */
  def requiredColl:Option[Collection] = None
  
  /**
   * Types can override this to provide default renderings when you look at a Property of this Type.
   */
  def renderProperty(prop:Property[_,_])(implicit request:RequestContext, state:SpaceState):Option[Future[Wikitext]] = None
  
  /**
   * The PType-math version of ==; this is here so that specific PTypes can override it.
   */
  def matches(left:ElemValue, right:ElemValue):Boolean = {
    doMatches(get(left), get(right))
  }
  def doMatches(left:VT, right:VT):Boolean = {
    left == right
  }
  
  /**
   * The PType that underlies this one. Mainly here to support the DelegatingType mechanism.
   */
  lazy val realType:PType[VT] = this
  
  /**
   * The usual width to show for this Type, with this Property, in Bootstrap's base-12 terms.
   * 
   * Defaults to 6, but any editable Type really ought to set this.
   */
  def editorSpan(prop:Property[_,_]):Int = 6
  
  /**
   * Should return true iff this PType can be coerced to the other.
   */
  def canCoerceTo(other:PType[_]):Boolean = false
  
  /**
   * Turn the given element of this Type into the other Type. This should only be called if you have
   * previously checked canCoerceTo!
   */
  def coerceTo(other:PType[_], elem:ElemValue):ElemValue = throw new Exception(s"PType $displayName can not be coerced to ${other.displayName}!")
}

/**
 * Late-resolving PType, used for those occasional cases that need late references to break circular cycles at
 * the start of time. This should be used *very* sparingly! This basically wraps around a real PType, but
 * late-resolves everything.
 */
class DelegatingType[VT](resolver: => PType[VT]) extends PType[VT](UnknownOID, UnknownOID, UnknownOID, () => emptyProps) {
  /**
   * Note that this is intentionally recursive, so it works with multiple layers of wrapping.
   */
  override lazy val realType:PType[VT] = resolver.realType
  
  def doDeserialize(v:String)(implicit state:SpaceState) = realType.doDeserialize(v)
  def doSerialize(v:VT)(implicit state:SpaceState) = realType.doSerialize(v)
  def doWikify(context:QLContext)(v:VT, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = 
    realType.doWikify(context)(v, displayOpt, lexicalThing)
  
  override def doMatches(left:VT, right:VT) = realType.doMatches(left, right)
  
  def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
    realType.renderInputXml(prop, context, currentValue, v)

  def doDefault(implicit state:SpaceState) = realType.doDefault
  
  override def canCoerceTo(other:PType[_]):Boolean = realType.canCoerceTo(other)
  override def coerceTo(other:PType[_], elem:ElemValue):ElemValue = realType.coerceTo(other, elem)
  
  override def toString = super.toString + ": " + realType.toString()
}

trait PTypeBuilderBase[VT, RT] {
  
  def pType:PType[VT]
  
  type rawType = RT
  
  def wrap(raw:RT):VT
  def apply(raw:RT):ElemValue = ElemValue(wrap(raw), pType)  
}
trait PTypeBuilder[VT, RT] extends PTypeBuilderBase[VT, RT] { this:PType[VT] =>
  def pType = this
}
trait SimplePTypeBuilder[VT] extends PTypeBuilder[VT, VT] { this:PType[VT] =>
  def wrap(raw:VT) = raw
}

/**
 * This side-trait can be declared by a Type; if used, it indicates that this Type wants to own the overall
 * rendering of the edit control, regardless of the containing Collection. Use this with care!
 */
trait FullInputRendering {
  def renderInputFull(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal):NodeSeq
}