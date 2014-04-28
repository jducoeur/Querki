package querki.uservalues

import scala.xml.NodeSeq

import models.{DisplayPropVal, OID, Property, PType, SimplePTypeBuilder, Wikitext}
import models.Thing.PropFetcher

import querki.core.TypeUtils.SystemType
import querki.ecology._
import querki.util.QLog
import querki.values.{ElemValue, QLContext, QValue, RequestContext, SpaceState}

case class OldUserValueWrapper[UVT,ST](userValue:Option[QValue], summary:Option[ST])

/**
 * Handle for a PType that descends from UserValueType. Lets you get at the bits.
 */
private[uservalues] trait TUserValue {
  /**
   * The PType that we are presenting to the user.
   */
  def userType:PType[_]
}

/**
 * A specialized kind of PType that presents two faces. To the user, it is a User Value,
 * where each User gets to enter their own Value of the underlying UVT. It also presents
 * an optional Summarizer of type VT, which is a summary of all of the User Values, and
 * which gets stored on the Thing itself.
 * 
 * Ratings and Reviews are the archetypal UserValueTypes.
 */
abstract class OldUserValueType[UVT, ST](tid:OID, pf:PropFetcher)(implicit e:Ecology) extends SystemType[OldUserValueWrapper[UVT,ST]](tid,pf) 
  with TUserValue with SimplePTypeBuilder[OldUserValueWrapper[UVT,ST]] 
{
  
  type Wrapper = OldUserValueWrapper[UVT,ST]
  
  /**
   * Note that these all operate on the *summary*, not the userValue itself.
   */
  def doDeserialize(ser:String)(implicit state:SpaceState):Wrapper = OldUserValueWrapper(None, Some(summarizer.doDeserialize(ser)))
  def doSerialize(v:Wrapper)(implicit state:SpaceState):String = v.summary.map(summarizer.doSerialize(_)).getOrElse("")
  def doWikify(context:QLContext)(v:Wrapper, displayOpt:Option[Wikitext] = None):Wikitext = 
    v.summary.map(summarizer.doWikify(context)(_, displayOpt)).getOrElse(Wikitext.empty)
  def doDefault(implicit state:SpaceState):Wrapper = OldUserValueWrapper(None, Some(summarizer.doDefault))
  override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
    <p><i>User Value Property -- use {prop.displayName}._edit to set the value.</i></p>
  
  override def wrappedValue(v:QValue):Option[QValue] = {
    v.firstAs(this) match {
      case Some(wrapper) => wrapper.userValue
      case None => {
        QLog.error("OldUserValueType.wrappedValue got handed a QValue that doesn't seem to be of this type: " + v)
        Some(v)
      }
    }
  }
  
  def wrapValue(uv:QValue, oldWrapperOpt:Option[QValue]):QValue = {
    val summaryOpt = {
      val wOpt = for {
        oldWrapperV <- oldWrapperOpt
        oldWrapper <- oldWrapperV.firstAs(this)
      }
        yield oldWrapper.summary
        
      wOpt.flatten
    }
    
    Core.ExactlyOne(this(OldUserValueWrapper(Some(uv), summaryOpt)))
  }
  
  // TODO: go through the rest of the PType methods, and see what else should be delegated to userType or summarizer
  
  /**
   * The PType of the actual value that each User is entering. Must be defined by the concrete class.
   */
  def userType:PType[UVT]
  
  /**
   * The Summarizer, which turns the individual UVTs into the summary VT. This is intentionally decomposed out, so
   * that we can potentially play plugin mix-and-match with it.
   * 
   * Must be defined by the concrete class.
   */
  def summarizer:Summarizer[UVT,ST]
}