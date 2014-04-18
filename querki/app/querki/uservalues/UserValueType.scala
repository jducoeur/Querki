package querki.uservalues

import scala.xml.NodeSeq

import models.{DisplayPropVal, OID, Property, PType, Wikitext}
import models.Thing.PropFetcher

import querki.core.TypeUtils.SystemType
import querki.ecology._
import querki.values.{ElemValue, QLContext, QValue, RequestContext, SpaceState}

case class UserValueWrapper[UVT,ST](userValue:Option[QValue], summary:Option[ST])

/**
 * A specialized kind of PType that presents two faces. To the user, it is a User Value,
 * where each User gets to enter their own Value of the underlying UVT. It also presents
 * an optional Summarizer of type VT, which is a summary of all of the User Values, and
 * which gets stored on the Thing itself.
 * 
 * Ratings and Reviews are the archetypal UserValueTypes.
 */
abstract class UserValueType[UVT, ST](tid:OID, pf:PropFetcher)(implicit e:Ecology) extends SystemType[UserValueWrapper[UVT,ST]](tid,pf) {
  
  type Wrapper = UserValueWrapper[UVT,ST]
  
  def extractUserDisplayPropVal(dpv:DisplayPropVal):DisplayPropVal = {
    val wrapperOpt = dpv.v.flatMap(_.firstAs(this))
    wrapperOpt.map(wrapper => dpv.copy(v = wrapper.userValue)).getOrElse(dpv)
  }
  
  /**
   * Note that these all operate on the *summary*, not the userValue itself.
   */
  def doDeserialize(ser:String)(implicit state:SpaceState):Wrapper = UserValueWrapper(None, Some(summarizer.doDeserialize(ser)))
  def doSerialize(v:Wrapper)(implicit state:SpaceState):String = v.summary.map(summarizer.doSerialize(_)).getOrElse("")
  def doWikify(context:QLContext)(v:Wrapper, displayOpt:Option[Wikitext] = None):Wikitext = 
    v.summary.map(summarizer.doWikify(context)(_, displayOpt)).getOrElse(Wikitext.empty)
  def doDefault(implicit state:SpaceState):Wrapper = UserValueWrapper(None, Some(summarizer.doDefault))
  // *Editing* a UserValueType means editing the underlying userType.
  override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
    userType.renderInputXml(prop, rc, extractUserDisplayPropVal(currentValue), v)
  
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