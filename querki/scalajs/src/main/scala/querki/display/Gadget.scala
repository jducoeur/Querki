package querki.display

import scala.scalajs.js

import org.scalajs.dom

import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.data.ThingInfo

/**
 * Wrapper around Scalatags, to provide support for tracking and updating the rendered elements
 * as the underlying data changes.
 */
trait Gadget[Output <: dom.Element] extends scalatags.jsdom.Frag {
  /**
   * Concrete subclasses should fill this in with the actual guts of the Gadget.
   */
  def doRender():scalatags.JsDom.TypedTag[Output]
  
  lazy val underlyingTag = doRender()
  
  /**
   * Utility, to make it easier to define data attributes.
   */
  def data(name:String):Attr = scalatags.generic.Attr(s"data-$name")

  /**
   * Utility function; this is often useful for wrapping complex expressions that produce Modifiers, which
   * often otherwise don't trigger the implicits properly. Often needed around if statements, in particular.
   */
  def MSeq(xs:Modifier*) = Vector[Modifier](xs)
  
  /**
   * Convenience function for composing classes in Gadgets and functions.
   */
  def classes(cs:Seq[String]) = cls:=cs.mkString(" ")
  
  /**
   * Show a standard Querki button, displaying whatever contents are given.
   */
  def querkiButton(show:Modifier, addlCls:Seq[String] = Seq.empty) =
    a(classes(Seq("btn", "btn-mini", "btn-primary", "_noPrint", "querki-icon-button") ++ addlCls),
      show)
  
  def icon(iconName:String) = i(classes(Seq("icon-white", s"icon-$iconName")))
  
  /**
   * Show a standard Querki icon button.
   */
  def iconButton(iconName:String, addlCls:Seq[String] = Seq.empty) = querkiButton(icon(iconName), addlCls)
      
  def spaceCall(call:js.Dynamic, params:js.Any*)(implicit ecology:Ecology):js.Dynamic = {
    val DataAccess = ecology.api[querki.data.DataAccess]
    call.apply((Seq[js.Any](DataAccess.userName, DataAccess.spaceId) ++ params):_*)
  }
  
  /**
   * Shortcut for fetching the URL of a Thing.
   */
  def thingUrl(thing:ThingInfo)(implicit ecology:Ecology) = {
    spaceCall(controllers.Application.thing, thing.urlName).url
  }
  
  var _elem:Option[Output] = None
  
  def render:Output = {
    val result = underlyingTag.render
    _elem = Some(result)
    result
  }
}
