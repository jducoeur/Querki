package querki.uservalues

import scala.xml.NodeSeq

import models.{DisplayPropVal, OID, Property, PType, Wikitext}
import models.Thing.PropFetcher

import querki.core.TypeUtils.SystemType
import querki.ecology._
import querki.values.{ElemValue, QLContext, RequestContext, SpaceState}

/**
 * A specialized kind of PType that presents two faces. To the user, it is a User Value,
 * where each User gets to enter their own Value of the underlying UVT. It also presents
 * an optional Summarizer of type VT, which is a summary of all of the User Values, and
 * which gets stored on the Thing itself.
 * 
 * Ratings and Reviews are the archetypal UserValueTypes.
 */
abstract class UserValueType[UVT, VT](tid:OID, pf:PropFetcher)(implicit e:Ecology) extends SystemType[VT](tid,pf) {
  
  /**
   * Note that these all operate on the *summary*, not the userValue itself.
   */
  def doDeserialize(ser:String)(implicit state:SpaceState):VT = summarizer.doDeserialize(ser)
  def doSerialize(v:VT)(implicit state:SpaceState):String = summarizer.doSerialize(v)
  def doWikify(context:QLContext)(v:VT, displayOpt:Option[Wikitext] = None):Wikitext = summarizer.doWikify(context)(v, displayOpt)
  def doDefault(implicit state:SpaceState):VT = summarizer.doDefault
  // You can't input summaries:
  override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = NodeSeq.Empty
  
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
  def summarizer:Summarizer[UVT,VT]
}