package querki.conversations

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import querki.data.ThingInfo
import querki.display.WrapperDiv
import querki.globals._

import messages._

class ConversationPane(
  val thingInfo: ThingInfo,
  focusedComment: Option[String]
)(implicit
  val ecology: Ecology
) extends Gadget[dom.HTMLDivElement]
     with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]

  implicit val t = thingInfo

  override def onCreate(e: dom.HTMLDivElement) = {
    val fut = Client[ConversationFunctions].getConversationsFor(thingInfo.oid).call()
    // TODO: how can we encapsulate this error-catching universally for Client? This needs research:
    fut.onFailure {
      case t: Throwable => println(s"Got an error: $t")
    }
    fut.foreach { convInfo =>
      val canComment = convInfo.canComment
      // If the user can't do anything with it, don't show the pane at all
      val showPane = canComment || (convInfo.canReadComments && (convInfo.convs.length > 0))

      if (showPane) {
        val convs =
          div(
            for (conv <- convInfo.convs)
              yield new ConversationGadget(Some(conv), canComment, t.oid)
          )
        convWrapper.replaceContents(convs.render)

        val guts =
          div(
            hr,
            h4(cls := "_commentsHeader", "Comments"),
            convWrapper,
            if (canComment)
              new NewConversationGadget(t.oid, convWrapper.elem)
          )

        allWrapper.replaceContents(guts.render)
        Gadgets.hookPendingGadgets()

        // If we're supposed to be focusing on a specific comment, show that:
        focusedComment.foreach { commentId =>
          val target = $(elem).find(s"a[name=$commentId]")
          // TODO: this highlight should probably fade out over, eg, three seconds?
          $(target).parent().addClass("_commentHighlight")
          $("html,body").scrollTop(target.offset().top)
        }
      }
    }
  }

  lazy val convWrapper = (new WrapperDiv()(ecology))(cls := "container")
  lazy val allWrapper = new WrapperDiv

  def doRender() = div(allWrapper)
}
