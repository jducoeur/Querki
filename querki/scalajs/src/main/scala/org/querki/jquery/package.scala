package org.querki

import scala.scalajs.js
import org.scalajs.dom
import dom.Element

package object jquery {
  /**
   * The main entry point into jQuery. We alias it to $, to match jQuery idiom.
   */
  val $ = JQueryStatic
  
  /**
   * A shorter alias for JQuery events, just to reduce keystrokes.
   */
  type JQEvt = JQueryEventObject
  
  implicit def builder2DialogOptions(builder:JQueryEventObjectBuilder) = builder._result

  implicit def jQuery2Ext(jq:JQuery):JQueryExtensions = new JQueryExtensions(jq)
  implicit def jQuery2Typed(jq:JQuery):JQueryTyped = new JQueryTyped(jq)

  // Union type enablers. This trick comes from this article:
  //
  //    http://cleverlytitled.blogspot.com/2009/03/disjoint-bounded-views-redux.html
  //
  // Knock on wood, it seems to suffice as a not-100%-general-but-good-enough approach
  // to constructing type unions for facade signatures. It's inefficient, unfortunately,
  // but not so much so as to be a big problem in most client-side code.
  //
  // NOTE: we originally tried doing pseudo-unions for facades using sealed classes that
  // extended js.Any. That turns out to work for simple situations but melts down in the
  // face of complexity: once you have several of them, you wind up with compile failures
  // due to ambiguity -- for instance, you wind up with multiple paths from String to
  // js.Any, and when you hit a function that really *does* take js.Any, the compiler gets
  // confused. Hence this less-efficient but more-robust approach instead.
  type or[A,B] = Either[A,B]
  implicit def l[T](t: T) = Left(t)  
  implicit def r[T](t: T) = Right(t)  
  implicit def ll[T](t: T) = Left(Left(t))  
  implicit def lr[T](t: T) = Left(Right(t))  
  implicit def lll[T](t: T) = Left(Left(Left(t)))    
  implicit def llr[T](t: T) = Left(Left(Right(t)))   
  implicit def llll[T](t: T) = Left(Left(Left(Left(t))))    
  implicit def lllr[T](t: T) = Left(Left(Left(Right(t))))
  
  def toJsAny[A, B, T <% or[A, B]](v: T):js.Any = {
    def rec(inner:Any):js.Any = {
      inner match {
        case o:or[_, _] => toJsAny(o)
        case x => x.asInstanceOf[js.Any]
      }
    }
    
    v match {
      case Left(l) => rec(l)
      case Right(r) => rec(r)
    }
  }
  
  /**
   * This is a particularly important union type. Selector is a common parameter type
   * in jQuery, meaning essentially a filter for choosing some elements. It can be a string
   * describing a kind of node (using a CSS-ish syntax), an Element, or an Array of Elements.
   * 
   * Note that the jQuery API documentation is *extremely* inconsistent about
   * how it treats the term "Selector" -- sometimes it just uses the term to mean Strings,
   * sometimes it means all of the possible types. So use this with some care.
   * 
   * TODO: many of the signatures in JQuery should be tweaked to use Selector, now that we've proved
   * that works!
   */
  type Selector = String or Element or Array[Element]
  
  /**
   * This union type gets used for several functions that really allow anything that can describe an
   * Element.
   */
  type ElementDesc = String or Element or JQuery or Array[Element]
}
