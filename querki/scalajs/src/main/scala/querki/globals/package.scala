package querki

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery

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
  val $ = jquery.jQuery
  
  type ClientEcot = querki.ecology.ClientEcot
  type Ecology = querki.ecology.Ecology
  type EcologyInterface = querki.ecology.EcologyInterface
  type EcologyMember = querki.ecology.EcologyMember
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:querki.ecology.InterfaceWrapperBase[querki.ecology.ClientState, querki.ecology.EcotImpl, T]):T = {
    wrapper.get
  }
  
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
  
  // These are improved signatures that can't simply be implicit, because they conflict with existing ones in the
  // jQuery facade.
  class JQFixes extends js.Object {
    def each(f:js.Function2[Int, dom.Element, Any]):jquery.JQuery = ???
    def map(f:js.Function2[Int, dom.Element, Any]):jquery.JQuery = ???
    def get():js.Array[_] = ???
  }
  implicit class JQFAdaptor(jq:jquery.JQuery) {
    // Note that jqf turns the jq *into* a jqf, rather than extending it, so that we can get around inference
    // problems:
    def jqf = jq.asInstanceOf[JQFixes]
  }
}
