package querki.conversations

import org.scalajs.dom.html
import org.scalajs.dom.raw.Element
import scalatags.JsDom.all.{input => inp, _}

import org.querki.gadgets._
import org.querki.jquery._

import querki.data.TID
import querki.globals._

private[conversations] class NewConversationGadget(
  val thingId: TID,
  target: => html.Div,
  startLinkTextOpt: Option[String] = None,
  startLinkClassOpt: Option[String] = None,
  replyPromptTextOpt: Option[String] = None,
  replyLinkClassOpt: Option[String] = None
)(implicit
  val ecology: Ecology
) extends Gadget[html.Div]
     with EcologyMember {

  lazy val replyGadget: ReplyGadget =
    new ReplyGadget(None, "Start a new conversation...", thingId, { node => onNewConversation(node) })
  lazy val shouldShowStartLink = startLinkTextOpt.isDefined
  lazy val startLinkClass = startLinkClassOpt.getOrElse("")
  lazy val startLinkText = startLinkTextOpt.getOrElse("Click to start a new conversation")
  lazy val startLink = a(cls := startLinkClass, onclick := { _: JQueryEventObject => showInput }, startLinkText)

  def showStartLink() = {
    $(elem).empty()
    $(elem).append(startLink.rendered)
  }

  def showInput() = {
    $(elem).empty()
    $(elem).append(replyGadget.rendered)
  }

  def reset() = {
    if (shouldShowStartLink)
      showStartLink()
    else
      showInput()
  }

  def doRender() = {
    div()
  }

  override def onCreate(e: html.Div) = {
    reset()
  }

  def onNewConversation(newNode: ConvNode) = {
    val convGadget = new ConversationGadget(Some(newNode), true, thingId, replyPromptTextOpt, replyLinkClassOpt)
    $(target).append(convGadget.render)
    if (shouldShowStartLink)
      showStartLink()
  }
}

object NewConversationGadget {

  def fromElem(e: Element)(implicit ecology: Ecology): NewConversationGadget = {
    val thingId = TID($(e).dataString("thingid"))
    val isAbove: Boolean = $(e).dataString("convwhere") == "above"
    val replyPromptTextOpt = $(e).data("replyprompt").toOption.map(_.asInstanceOf[String])
    val replyLinkClassOpt = $(e).data("replylinkclass").toOption.map(_.asInstanceOf[String])
    val startLinkTextOpt = $(e).data("startlinkprompt").toOption.map(_.asInstanceOf[String])
    val startLinkClassOpt = $(e).data("startlinkclass").toOption.map(_.asInstanceOf[String])
    lazy val gadget: NewConversationGadget =
      new NewConversationGadget(
        thingId, {
          val newConv = div().render
          if (isAbove)
            $(gadget.elem).before(newConv)
          else
            $(gadget.elem).after(newConv)
          newConv
        },
        startLinkTextOpt,
        startLinkClassOpt,
        replyPromptTextOpt,
        replyLinkClassOpt
      )
    $(e).replaceWith(gadget.rendered)
    gadget
  }
}
