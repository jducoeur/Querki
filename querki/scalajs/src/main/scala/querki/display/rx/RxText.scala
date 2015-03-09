package querki.display.rx

import org.scalajs.dom.{raw => dom}
import org.scalajs.jquery._
import scalatags.JsDom.all._
import rx._
import rx.ops._

import querki.globals._

import querki.display.Gadget

/**
 * A reactive wrapper around a text input. It is considered to have a value only iff the field is non-empty.
 */
class RxText(mods:Modifier*) extends Gadget[dom.HTMLInputElement] {
  
  private def curValue =
    for {
      e <- elemOpt
      v = $(e).valueString
      if (v.length > 0)
    }
      yield v
  
  lazy val textOpt = Var[Option[String]](curValue)
  
  def doRender() = input(tpe:="text", mods)
  
  def setValue(v:String) = {
    $(elem).value(v)
    update()
  }
  
  private def update() = { textOpt() = curValue }
  
  override def onCreate(e:dom.HTMLInputElement) = {
    $(e).change({ evt:JQueryEventObject => update() })
    update()
  }
}
