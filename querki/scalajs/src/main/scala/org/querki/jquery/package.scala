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

  // "Type or" -- union type enablers. This trick comes from this article:
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
  //
  // TODO: conceptually, this really belongs in the org.querki.jsext library, since it is
  // a general-purpose mechanism. However, I do *not* want to require calling code to have
  // to include jsext in order to get the imports. We could create a trait that we mix into
  // the jquery package, but I am concerned whether using it in other packages as well might
  // produce implicit conflicts. This therefore needs some experimentation, to see whether
  // it is safe...
  type tor[A,B] = Either[A,B]
  implicit def l[T](t: T) = Left(t)  
  implicit def r[T](t: T) = Right(t)  
  implicit def ll[T](t: T) = Left(Left(t))  
  implicit def lr[T](t: T) = Left(Right(t))  
  implicit def lll[T](t: T) = Left(Left(Left(t)))    
  implicit def llr[T](t: T) = Left(Left(Right(t)))   
  implicit def llll[T](t: T) = Left(Left(Left(Left(t))))    
  implicit def lllr[T](t: T) = Left(Left(Left(Right(t))))
  
  /**
   * Given a parameter of a "tor"ed type, extract the actual value and cast it to js.Any. You
   * use this in a strongly-typed mid-level facade, and pass the result into a weakly-typed
   * low-level facade.
   */
  def toJsAny[A, B, T <% tor[A, B]](v: T):js.Any = {
    def rec(inner:Any):js.Any = {
      inner match {
        case o:tor[_, _] => toJsAny(o)
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
   * TBD: the js.Array[Element] here is kind of a pain in the butt from a Scala POV, both
   * because it often requires converting a Scala Seq into a js.Array at the call site, and
   * because Array is invariant, so a Sequence of a subclass of Element can't be used here.
   * We should see whether we can do something clever so that these accept Seq[+Element], which
   * would be far more idiomatic Scala. (See Querki's ConversationPane.onNewComment() as a good
   * test to play with.)
   */
  type Selector = String tor Element tor js.Array[Element]
  
  /**
   * This union type gets used for several functions that really allow anything that can describe an
   * Element. This is similar to Selector, but allows you to pass in a JQuery as well.
   */
  type ElementDesc = String tor Element tor JQuery tor js.Array[Element]
  
  /**
   * This union type represents valid types that you can set an attribute to.
   */
  type AttrVal = String tor Int tor Boolean
}
