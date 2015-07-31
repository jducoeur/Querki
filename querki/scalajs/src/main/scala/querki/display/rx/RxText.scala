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
class RxInput(charFilter:Option[JQueryEventObject => Boolean], inputType:String, mods:Modifier*) extends Gadget[dom.HTMLInputElement] {
  
  def this(inputType:String, mods:Modifier*) = this(None, inputType, mods)
  
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
    // If a charFilter was specified, run each keystroke past it as a legality check:
    $(e).keydown { evt:JQueryEventObject =>
      charFilter match {
        case Some(filter) => {
          val allowed = filter(evt)
          println(s"${evt.which.toChar} = $allowed")
          if (!allowed)
            evt.preventDefault()
          allowed
        }
        case None => true
      }
    }
    // We use input instead of change, so that we fire on every keystroke:
    $(e).on("input", { e:dom.Element => update() })
    update()
  }
}

class RxText(mods:Modifier*) extends RxInput("text", mods)
