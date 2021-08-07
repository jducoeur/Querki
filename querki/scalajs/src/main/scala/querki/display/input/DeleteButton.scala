package querki.display.input

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all.{input => inp, _}
import org.querki.facades.bootstrap._

import querki.display.HookedGadget
import querki.globals._

class DeleteButton(doDelete: () => Unit)(implicit e: Ecology) extends HookedGadget[dom.html.Span](e) {
  def doRender() = span(cls := "_deleteCommentButton", "x")

  def hook() = {
    $(elem).on("click", confirmDelete)
  }

  lazy val confirmDelete: Function1[JQueryEventObject, js.Any] = { (evt: JQueryEventObject) =>
    val deleteButton = $(elem)
    deleteButton.popover(
      PopoverOptions.content("Click again to delete").placement(Position.left).trigger(Trigger.manual)
    )
    deleteButton.popover(PopoverCommand.show)
    deleteButton.off("click")
    deleteButton.on("click", reallyDelete)
    dom.window.setTimeout(
      { () =>
        deleteButton.popover(PopoverCommand.hide)
        deleteButton.off("click")
        deleteButton.on("click", confirmDelete)
      },
      2000
    )
  }

  lazy val reallyDelete: Function1[JQueryEventObject, js.Any] = { (evt: JQueryEventObject) =>
    doDelete()
  }
}

class DeleteInstanceButton(doDelete: () => Unit)(implicit e: Ecology) extends DeleteButton(doDelete)(e) {

  override def doRender() = span(
    cls := "_deleteInstanceButton",
    i(title := "Click to delete this", cls := "glyphicon glyphicon-trash _withTooltip")
  )
}

/**
 * Note that this is *not* a constructor for Delete buttons in general. That would be
 * nonsensical: what the delete button does depends on context. This one is specific to
 * the _deleteInstanceButton class.
 */
object DeleteInstanceButton extends querki.display.QuerkiUIUtils {
  import autowire._
  import querki.api.ThingFunctions

  def apply(rawElement: dom.html.Span)(implicit e: Ecology) = {
    (new DeleteInstanceButton(deleteFunc(rawElement, e))).setElem(rawElement)
  }

  def deleteFunc(
    button: dom.Element,
    e: Ecology
  ) = { () =>
    val editor = $(button).parents("._instanceEditor")
    val thingId = editor.tidString("thingid")
    val Client = e.api[querki.client.Client]
    Client[ThingFunctions].deleteThing(thingId).call().foreach { dummy =>
      editor.hide(400, { () => editor.remove() })
    }
  }
}
