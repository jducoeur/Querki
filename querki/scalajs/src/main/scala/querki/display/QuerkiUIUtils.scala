package querki.display

import org.scalajs.dom

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag

import org.querki.jquery._

import models.Wikitext
import querki.data._
import querki.util.ScalatagUtils

trait QuerkiUIUtils extends ScalatagUtils {
  /**
   * Render some wikitext from the server.
   * 
   * This should be changing rapidly and dramatically, becoming a much more complex Gadget unto itself.
   */
  def wikitext(w:Wikitext) = raw(w.display.html.toString)
  
  /**
   * Show a standard Querki button, displaying whatever contents are given.
   */
  def querkiButton(show:Modifier, addlCls:Seq[String] = Seq.empty) = plainQuerkiButton(show, "btn-primary" +: addlCls)
  def plainQuerkiButton(show:Modifier, addlCls:Seq[String] = Seq.empty) =
    a(classes(Seq("btn", "btn-default", "btn-sm", "_noPrint", "querki-icon-button") ++ addlCls),
      show)
  
  def icon(iconName:String) = i(classes(Seq("glyphicon", s"glyphicon-$iconName")))
  def faIcon(iconName:String) = i(classes(Seq("fa", s"fa-$iconName fa-lg")))
  
  /**
   * Show a standard Querki icon button.
   */
  def iconButton(iconName:String, addlCls:Seq[String] = Seq.empty) = plainQuerkiButton(icon(iconName), addlCls)
  def faIconButton(iconName:String, addlCls:Seq[String] = Seq.empty) = plainQuerkiButton(faIcon(iconName), addlCls)
  
  def thingUrl(thing:BasicThingInfo) = s"#!${thing.urlName.underlying}"
  
  def thingUrl(name:TID) = s"#!${name.underlying}"
  
  /**
   * A standard link to a Thing, if you're not trying to do anything odd with it.
   */
  def thingLink(thing:BasicThingInfo):TypedTag[dom.html.Anchor] =
    a(href:=thingUrl(thing), thing.displayName)
    
  implicit class jqGadgetExts(jq:JQuery) {
    def tidString(name:String) = TID(jq.data(name).asInstanceOf[String])
  }

}
