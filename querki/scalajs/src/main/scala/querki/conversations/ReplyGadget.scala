package querki.conversations

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import org.querki.gadgets._
import org.querki.jquery._
import querki.display.input.AutosizeFacade._

import querki.globals._

import messages._

class ReplyGadget(
  replyTo: Option[CommentId],
  ph: String,
  thingId: TID,
  onPosted: ConvNode => Unit
)(implicit
  val ecology: Ecology
) extends Gadget[dom.HTMLDivElement]
     with EcologyMember {
  lazy val Client = interface[querki.client.Client]

  override def onCreate(e: dom.HTMLDivElement) = {
    $(commentInput.elem).autosize()
  }

  def postComment(): Unit = {
    Client[ConversationFunctions].addComment(
      thingId,
      $(commentInput.elem).value().asInstanceOf[String],
      replyTo
    ).call().foreach { node =>
      $(commentInput.elem).value("")
      onPosted(node)
    }
  }

  lazy val commentInput = Gadget(textarea(cls := "_commentInput form-control", placeholder := ph))

  def doRender() =
    div(
      cls := "_addComment row",
      div(
        cls := "col-md-11",
        commentInput,
        inp(
          cls := "_postCommentButton btn btn-info btn-sm",
          tpe := "button",
          value := "Post Comment",
          onclick := { () => postComment() }
        )
      )
    )
}
