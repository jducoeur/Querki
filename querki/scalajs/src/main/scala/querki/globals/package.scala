package querki

import scala.scalajs.js
import org.scalajs.dom
import dom.Element
import org.scalajs.jquery._

/**
 * This package provides the "global imports" that are commonly used across the client. It
 * is specifically intended that most files will say:
 * {{{
 * import querki.globals._
 * }}}
 * It should *not* include the kitchen sink, but if an import is used in more than half of
 * all files, it belongs here. IMPORTANT: overuse of this mechanism will slow compile times,
 * so be thoughtful about what belongs here!
 */
package object globals {
  
  type JSExport = js.annotation.JSExport
  
  val lit = js.Dynamic.literal
  
  /**
   * The main entry point into jQuery. I alias it to $, to match jQuery idiom.
   */
  val $ = jQuery
  
  type ClientEcot = querki.ecology.ClientEcot
  type Ecology = querki.ecology.Ecology
  type EcologyInterface = querki.ecology.EcologyInterface
  type EcologyMember = querki.ecology.EcologyMember
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:querki.ecology.InterfaceWrapperBase[querki.ecology.ClientState, querki.ecology.EcotImpl, T]):T = {
    wrapper.get
  }
  
  /**
   * This allows you to use a TypedTag in any context where a Gadget is expected.
   */
  implicit def tag2Gadget[Output <: dom.Element](guts:scalatags.JsDom.TypedTag[Output]) = new querki.display.TypedGadget[Output](guts, { elem:Output => })
  
  /**
   * The standard implicit ExecutionContext for Futures. Provide one explicitly if you want to do something different.
   * 
   * This should generally be left as runNow; otherwise, it can foul up utest.
   */
  implicit val queue = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
  
  // I'm now using Future and Promise enough that we may as well make them generally available
  type Future[T] = scala.concurrent.Future[T]
  val Future = scala.concurrent.Future
  type Promise[T] = scala.concurrent.Promise[T]
  val Promise = scala.concurrent.Promise
  
  /**
   * My current tweaks to the main jQuery facade. Everything here should be considered experimental, and a candidate
   * for a PR to the main facade.
   */
  class JQExt extends js.Object {
    def each(func:js.ThisFunction0[Element, Any]):JQuery = ???
    def each(func:js.ThisFunction1[Element, Int, Any]):JQuery = ???
    def map(func:js.ThisFunction0[Element, Any]):JQuery = ???
    def map(func:js.ThisFunction1[Element, Int, Any]):JQuery = ???
    def click(func:js.ThisFunction1[Element, JQueryEventObject, Any]):JQuery = ???
  }
  implicit def jq2Ext(jq:JQuery):JQExt = jq.asInstanceOf[JQExt]
  
  /**
   * These are genuine extensions to jQuery -- useful higher-level functions, which mostly tighten up the types.
   */
  implicit class jqExt2(jq:JQuery) {
    // The value of this Element; use this when it can only make sense as a String in context:
    def valueString = jq.value().asInstanceOf[String]
  }
  
  // These are improved signatures that can't simply be implicit, because they conflict with existing ones in the
  // jQuery facade.
  class JQFixes extends js.Object {
    def get():js.Array[_] = ???
    // Height is always in px, right? And can never be non-integer, correct?
    def height():Int = ???
  }
  implicit class JQFAdaptor(jq:JQuery) {
    // Note that jqf turns the jq *into* a jqf, rather than extending it, so that we can get around inference
    // problems:
    def jqf = jq.asInstanceOf[JQFixes]
  }
  
  /**
   * A quick-and-dirty temp wrapper to inject heavy spewage around some code while debugging.
   */
  def spewing[T](msg:String)(f: => T):T = {
    println(s"Trying $msg")
    try {
      val result = f
      println(s"  $msg succeeded")
      result
    } catch {
      case ex:Exception => { println(s"  $msg failed: $ex"); ex.printStackTrace(); throw ex }
    }
  }
}
