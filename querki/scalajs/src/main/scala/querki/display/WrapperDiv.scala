package querki.display

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

/**
 * A higher-level view of WrapperDiv, to encapsulate the common pattern "show a spinner until the given Future
 * pans out, then process the results and replace the display".
 * 
 * DEPRECATED: this should be replaced with RxDiv more or less everywhere.
 */
class AfterLoading[T, Output <: dom.Element](fut:Future[T])(guts:T => Gadget[Output]) extends MetaGadget[dom.HTMLDivElement] {
  // TODO: once we upgrade to Bootstrap 3, we should switch to FontAwesome and use the spinners in that:
  lazy val wrapper = (new WrapperDiv).initialContent("Loading...")
  
  def doRender() = wrapper
  
  fut.map { result =>
    val finalTag = guts(result)
    wrapper.replaceContents(finalTag.render)
  }
}
object AfterLoading {
  def apply[T, Output <: dom.Element](fut:Future[T])(guts:T => Gadget[Output]) = new AfterLoading(fut)(guts)
}

/**
 * This is a trivial placeholder div element, for use when you want to be able to replace the
 * content of the div easily.
 * 
 * Note that AfterLoading encapsulates the combination of WrapperDiv with a Future.
 * 
 * DEPRECATED: this should be replaced with RxDiv more or less everywhere.
 */
class WrapperDiv extends Gadget[dom.HTMLDivElement] {
  
  var contentsOpt:Option[dom.Element] = None
  
  // Yes, using a var here makes me twitch. Scalatags avoids that by returning a mutated version of this
  // object in apply() instead, which is lovely in principle, but the entire point of the exercise is that
  // I want to be able to retain a handle to this so that I can mutate it later. This probably suggests
  // that the entire architecture is suspect, and that there is a pure-functional version we should be
  // using instead, but I don't have my head around that yet.
  var modifiers:Seq[Modifier] = Seq.empty
  
  var initial:Seq[Modifier] = Seq.empty
  
  def apply(xs:Modifier*):WrapperDiv = {
    modifiers = xs
    this
  }

  /**
   * Use this to specify temporary initial content, that will be later replaced.
   */
  def initialContent(xs:Modifier*):WrapperDiv = {
    initial = xs
    this
  }
  
  def doRender = contentsOpt match {
    case Some(contents) => div(modifiers :+ bindNode(contents)) 
    case None => div(modifiers ++ initial)
  } 

  def replaceContents(newContent:dom.Element) = {
    contentsOpt = Some(newContent)
    _elem match {
      case Some(elem) => {
        $(elem).empty
        $(elem).append(newContent)
        $(elem).change()
        this
      }
      case None => { this }
    }
  }
}
