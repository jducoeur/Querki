package querki.display

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

/**
 * This is a trivial placeholder div element, for use when you want to be able to replace the
 * content of the div easily.
 */
class WrapperDiv extends Gadget[dom.HTMLDivElement] {
  
  var contentsOpt:Option[dom.Element] = None
  
  // Yes, using a var here makes me twitch. Scalatags avoids that by returning a mutated version of this
  // object in apply() instead, which is lovely in principle, but the entire point of the exercise is that
  // I want to be able to retain a handle to this so that I can mutate it later. This probably suggests
  // that the entire architecture is suspect, and that there is a pure-functional version we should be
  // using instead, but I don't have my head around that yet.
  var modifiers:Seq[Modifier] = Seq.empty
  
  def apply(xs:Modifier*):WrapperDiv = {
    modifiers = xs
    this
  }
  
  def doRender = contentsOpt match {
    case Some(contents) => div(modifiers :+ bindNode(contents)) 
    case None => div(modifiers)
  } 

  def replaceContents(newContent:dom.Element) = {
    contentsOpt = Some(newContent)
    _elem match {
      case Some(elem) => {
        $(elem).empty
        $(elem).append(newContent)
        $(elem).change()
      }
      case None => {}
    }
  }
}
