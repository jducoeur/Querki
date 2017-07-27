package querki

import scala.scalajs.js
import js.annotation.JSName

import org.scalajs.dom
import dom.Element

import org.querki.jquery
import jquery._
import org.querki.gadgets.core._

import scalatags.JsDom.TypedTag

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
   * Ecology Types that are common enough to belong here.
   */
  type ClientEcot = querki.ecology.ClientEcot
  type Ecology = querki.ecology.Ecology
  type EcologyInterface = querki.ecology.EcologyInterface
  type EcologyMember = querki.ecology.EcologyMember
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:querki.ecology.InterfaceWrapperBase[querki.ecology.ClientState, querki.ecology.EcotImpl, T]):T = {
    wrapper.get
  }
  
  type TID = querki.data.TID
  val TID = querki.data.TID
  implicit def thingInfo2TID(info:querki.data.BasicThingInfo):TID = info.oid
  
  /**
   * This implicit is needed for GadgetRef's reassign functions.
   * 
   * TODO: at the moment, this is creating an instance each time, which is a pointless amount of
   * work. Can we create a single stable instance? The problem is that it depends on the Ecology.
   * Worse came to worst we could memoize that, but it's pretty evil to stuff the ecology into the
   * global world.
   */
  implicit def ecologyGadgetNotifier(implicit e:Ecology) = new org.querki.gadgets.core.GadgetNotifier[Ecology] {
    def layoutChanged[Output <: org.scalajs.dom.html.Element](g:Gadget[Output]):Unit = {
      lazy val Pages = e.api[querki.pages.Pages]
      Pages.updatePage(g)
    }
  }
  
  // TODO: this is now duplicated in the Gadgets library. Can I get rid of it here?
  implicit def tag2Gadget[Output <: dom.html.Element](
    guts:TypedTag[Output]
  ):Gadget[Output]
    = new TypedGadget[Output](guts)
  
  /**
   * The standard implicit ExecutionContext for Futures. Provide one explicitly if you want to do something different.
   * 
   * This used to be runNow, but that's now strongly discouraged. This is the only context that sjrd says should be used.
   */
  implicit val execContext = scala.concurrent.ExecutionContext.global
  
  // I'm now using Future and Promise enough that we may as well make them generally available
  type Future[T] = scala.concurrent.Future[T]
  val Future = scala.concurrent.Future
  type Promise[T] = scala.concurrent.Promise[T]
  val Promise = scala.concurrent.Promise
  
  /**
   * A quick-and-dirty temp wrapper to inject heavy spewage around some code while debugging.
   */
  def spewing[T](msg:String)(f: => T):T = {
    println(s"Trying $msg")
    try {
      val result = f
      println(s"  $msg succeeded, returning $result")
      result
    } catch {
      case ex:Exception => { println(s"  $msg failed: $ex"); ex.printStackTrace(); throw ex }
    }
  }
  def spew(msg:String) = println(msg)
  def spew(msg:String, obj:js.Object) = println(msg + "\n" + sobj(obj))
  def sobj(obj:js.Object) = js.JSON.stringify(obj)
}
