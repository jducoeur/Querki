package querki.conversations

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import moment._

import querki.globals._

import querki.data.ThingInfo
import querki.display.{Gadget, QText, WrapperDiv}

class ConversationPane(val thingInfo:ThingInfo)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  
  override def onCreate(e:dom.HTMLDivElement) = {
    val fut = Client[ConversationFunctions].getConversationsFor(thingInfo.oid).call()
    // TODO: how can we encapsulate this error-catching universally for Client? This needs research:
    fut.onFailure {
      case t:Throwable => println(s"Got an error: $t")
    }
    fut.foreach { convInfo =>
      val guts = 
        div(
          hr,
          h4(cls:="_commentsHeader", "Comments"),
          for (conv <- convInfo.convs)
            yield new ConversationGadget(conv, convInfo.canComment)
        )
      convWrapper.replaceContents(guts.render)
      InputGadgets.hookPendingGadgets()
    }
  }
  
  lazy val convWrapper = new WrapperDiv
  
  def doRender() = div(convWrapper)
}

private [conversations] class CommentGadget(comment:CommentInfo)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  val cid = comment.id
  val created = moment(comment.createTime).calendar()
  
  def doRender() =
    div(
      cls:="_comment",
      id:=s"_comment$cid",
      a(cls:="_commentLink", name:=s"comment$cid"),
      if (comment.canDelete) {
        // TODO: deal with deleting
        span(cls:="_deleteCommentButton", "x")
      },
      div(cls:="_commentHeader",
        span(cls:="_commentAuthor", comment.author.name),
        span(cls:="_commentTime", created)
      ),
      new QText(comment.content, cls:="_commentText")
    )
}

private [conversations] class ConversationGadget(conv:ConvNode, canComment:Boolean)(implicit val ecology:Ecology) 
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{
  def flattenNodes(node:ConvNode):Seq[CommentGadget] = {
    new CommentGadget(node.comment) +: node.responses.flatMap(flattenNodes(_))
  }
  
  def doRender() =
    div(
      cls:="_convThread row-fluid",
      div(cls:="_commentContainer offset1 span9",
        flattenNodes(conv)
      ),
      if (canComment) {
        div(cls:="_replyContainer offset1 span9",
          // TODO: deal with replying
          inp("_replyPlaceholder", tpe:="text", placeholder:="Click here to reply...")
        )
      }
    )
}
