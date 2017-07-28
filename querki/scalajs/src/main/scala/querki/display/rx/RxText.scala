package querki.display.rx

import org.scalajs.dom
import dom.html.Element
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._
import org.querki.gadgets._

import querki.globals._

import querki.display.input.AutosizeFacade._

/**
 * Some common functionality between RxText and RxTextArea.
 */
trait RxTextBase[E <: Element] extends Gadget[E] {
  implicit def ctx:Ctx.Owner
  
  protected def curValue =
    for {
      e <- elemOpt
      v = $(e).valueString
      if (v.length > 0)
    }
      yield v
  
  lazy val textOpt = Var[Option[String]](curValue)
  lazy val text = Rx { textOpt().getOrElse("") }
  
  protected def update() = { textOpt() = curValue }
  
  def setValue(v:String) = {
    $(elem).value(v)
    update()
  }
      
  def length = textOpt.now.map(_.length()).getOrElse(0)
}

/**
 * A reactive wrapper around a text input. It is considered to have a value only iff the field is non-empty.
 */
class RxInput(charFilter:Option[(JQueryEventObject, String) => Boolean], inputType:String, mods:Modifier*)
  (implicit val ecology:Ecology, val ctx:Ctx.Owner) 
  extends RxTextBase[dom.html.Input]
{
  def this(inputType:String, mods:Modifier*)(implicit ecology:Ecology, ctx:Ctx.Owner) = this(None, inputType, mods)
  
  def doRender() = input(tpe:=inputType, mods)
  
  override def onCreate(e:dom.html.Input) = {
    // If a charFilter was specified, run each keystroke past it as a legality check:
    $(e).keydown { evt:JQueryEventObject =>
      val which = evt.which
      val fOpt = enterFunc.now
      
      if (which == 13 && fOpt.isDefined) {
        // They hit Enter, and we are doing something special with Enter:
        fOpt.get(text.now)
        evt.preventDefault()
        false
      } else { 
        // Is this character allowed?
        charFilter match {
          case Some(filter) => {
            val allowed = filter(evt, text.now)
            if (!allowed)
              evt.preventDefault()
            allowed
          }
          case None => true
        }
      }
    }
    // We use input instead of change, so that we fire on every keystroke:
    $(e).on("input", { e:dom.Element => update() })
    update()
  }
  
  val enterFunc = Var[Option[String => Unit]](None)
  
  def onEnter(f:String => Unit) = enterFunc() = Some(f)
}

class RxText(mods:Modifier*)(implicit e:Ecology, ctx:Ctx.Owner) extends RxInput("text", mods)

class RxTextArea(mods:Modifier*)(implicit val ecology:Ecology, val ctx:Ctx.Owner)
  extends RxTextBase[dom.html.TextArea]
{
  def doRender() = textarea(mods)
  
  override def onCreate(e:dom.html.TextArea) = {
    $(e).on("input", { e:dom.Element => update() })
    $(e).autosize()
  }
}
