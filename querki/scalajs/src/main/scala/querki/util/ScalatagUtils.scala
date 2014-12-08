package querki.util

import scalatags.JsDom.all._

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
  
}

object ScalatagUtils extends ScalatagUtils
