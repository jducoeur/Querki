package querki

// TODO: this is unfortunate abstraction leakage:
import play.api.templates.Html

import scala.xml.Elem

import play.api.data.Form

import querki.ecology._

import models.{DisplayPropVal, FieldIds, FormFieldInfo, Property, Thing, Wikitext}

import querki.ui.UIRenderer
import querki.values.{QLContext, QValue, RequestContext, SpaceState}

package object html {
  object RenderSpecialization extends Enumeration {
    type RenderSpecialization = Value
  
    val Unspecialized, PickList, WithAdd = Value
  }

  /**
   * This is currently the HTML-specific interface for rendering. We should gradually move towards exposing
   * querki.ui.UIRenderer, which is implementation-agnostic.
   */
  trait HtmlRenderer extends UIRenderer with EcologyInterface {
    def addClasses(elem:Elem, addedClasses:String):Elem
    def propValFromUser(prop:Property[_,_], on:Option[Thing], form:Form[_], context:QLContext, containers:Option[FieldIds]):FormFieldInfo
    def renderPropertyInput(rc:RequestContext, prop:Property[_,_], 
        currentValue:DisplayPropVal, 
        specialization:Set[RenderSpecialization.RenderSpecialization] = Set(RenderSpecialization.Unspecialized)):Html
  }
  
  trait HtmlUI extends EcologyInterface {
    def HtmlValue(html:Html):QValue
    def HtmlValue(str:String):QValue
    def HtmlValue(xml:Elem):QValue
    
    def toWikitext(xml:Elem):Wikitext
  }
}
