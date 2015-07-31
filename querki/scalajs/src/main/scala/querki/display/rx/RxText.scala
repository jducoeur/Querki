package querki.display.rx

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.Gadget

/**
 * A reactive wrapper around a text input. It is considered to have a value only iff the field is non-empty.
 */
class RxInput(inputType:String, mods:Modifier*) extends Gadget[dom.HTMLInputElement] {
  
  private def curValue =
    for {
      e <- elemOpt
      v = $(e).valueString
      if (v.length > 0)
    }
      yield v
  
  lazy val textOpt = Var[Option[String]](curValue)
  lazy val text = Rx { textOpt().getOrElse("") }
  
  def doRender() = input(tpe:=inputType, mods)
  
  def setValue(v:String) = {
    $(elem).value(v)
    update()
  }
      
  def length = textOpt().map(_.length()).getOrElse(0)
  
  private def update() = { textOpt() = curValue }
  
  override def onCreate(e:dom.HTMLInputElement) = {
    // We use input instead of change, so that we fire on every keystroke:
    $(e).on("input", { e:dom.Element => update() })
    update()
  }
}

class RxText(mods:Modifier*) extends RxInput("text", mods)