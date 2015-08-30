package querki

// TODO: this is unfortunate abstraction leakage:
//import play.twirl.api.Html

import scala.xml.NodeSeq

import play.api.data.Form

import querki.ecology._

import models.{DisplayPropVal, FieldIds, FormFieldInfo, OID, Property, Thing, ThingId, Wikitext}

import querki.core.QLText
import querki.ui.UIRenderer
import querki.values.{QLContext, QValue, RequestContext, SpaceState}

package object html {
  /**
   * The Html type. Anything marked as Html is, by definition, safe to render directly
   * with no escaping!
   * 
   * Note that the definition of this type is different between the client and server!
   */
//  type Html = play.twirl.api.Html
//  def Html(str:String) = play.twirl.api.Html(str)
  
  object RenderSpecialization extends Enumeration {
    type RenderSpecialization = Value
  
    val Unspecialized, PickList, WithAdd, FromEditFunction = Value
  }

  /**
   * This is currently the HTML-specific interface for rendering. We should gradually move towards exposing
   * querki.ui.UIRenderer, which is implementation-agnostic.
   */
  trait HtmlRenderer extends UIRenderer with EcologyInterface {
    // If nodes is itself an Elem, this uses that; otherwise, it expects nodes to be a sequence of Elems:
    def addClasses(nodes:NodeSeq, addedClasses:String):NodeSeq
    def propValFromUser(fieldIds:FieldIds, on:Option[Thing], form:Form[_], context:QLContext):FormFieldInfo
    def propValFromUser(fieldIds:FieldIds, vs:List[String], context:QLContext):FormFieldInfo
    def renderPropertyInput(context:QLContext, prop:Property[_,_], 
        currentValue:DisplayPropVal, 
        specialization:Set[RenderSpecialization.RenderSpecialization] = Set(RenderSpecialization.Unspecialized)):Html
    def renderPropertyInputStr(context:QLContext, prop:Property[_,_], 
        currentValue:DisplayPropVal, 
        specialization:Set[RenderSpecialization.RenderSpecialization] = Set(RenderSpecialization.Unspecialized)):String
  }
  
  trait HtmlUI extends EcologyInterface {
    def PageHeaderProperty:Property[QLText,String]
    
    def HtmlValue(html:Html):QValue
    def HtmlValue(str:String):QValue
    def HtmlValue(xml:NodeSeq):QValue
    
    def toWikitext(xml:NodeSeq):Wikitext
  }
  
  // Note that, while this interface is defined here, it is actually implemented in controllers. It exists so
  // that code "inside the onion" doesn't need to know about controllers, and particularly not about the reverse router.
  trait PublicUrls extends EcologyInterface {
    /**
     * Returns the URL to create a new Instance of the specified Model and begin editing it.
     */
    def createAndEditUrl(rc:RequestContext, modelId:ThingId)(implicit state:SpaceState):String
  }
}
