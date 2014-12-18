package querki.util

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

/**
 * Convenience utilities to use with Scalatags. You can either mix this into your class, or use the
 * companion object.
 */
trait ScalatagUtils {
  
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
   * Utility function; this is often useful for wrapping complex expressions that produce Frags, which
   * often otherwise don't trigger the implicits properly. Often needed around if statements, in particular.
   */
  def FSeq(xs:Frag*) = Vector[Frag](xs)  
  
  /**
   * Convenience function for composing classes in Gadgets and functions.
   */
  def classes(cs:Seq[String]) = cls:=cs.mkString(" ")
  
  /**
   * Base class for teaching Scalatags how to use a BasicThingInfo as an Attribute Value. We need one
   * of these for each subclass of BasicThingInfo. This automatically uses the TID of the Thing as
   * the actual value.
   */
  class BasicThingAttr[T <: querki.data.BasicThingInfo] extends scalatags.JsDom.AttrValue[T] {
    override def apply(t:dom.Element, a:Attr, v:T) = {
      t.setAttribute(a.name, v.oid.underlying)
    }
  }
  implicit val BasicThingAttr = new BasicThingAttr[querki.data.BasicThingInfo]
  implicit val ThingAttr = new BasicThingAttr[querki.data.ThingInfo]
  implicit val PropAttr = new BasicThingAttr[querki.data.PropInfo]
  implicit val TypeAttr = new BasicThingAttr[querki.data.TypeInfo]
}

object ScalatagUtils extends ScalatagUtils
