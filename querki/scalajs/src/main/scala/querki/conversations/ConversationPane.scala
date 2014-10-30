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
import querki.display.input.AutosizeFacade._

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

class ReplyGadget(ph:String)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  override def onCreate(e:dom.HTMLDivElement) = {
    $(elem).find("._commentInput").autosize()
  }
  
  def focus() = $(elem).find("._commentInput").focus()
    
  def doRender() =
    div(cls:="_addComment row-fluid",
      div(cls:="span11",
        textarea(cls:="_commentInput", placeholder:=ph),
        inp(cls:="_postCommentButton btn btn-info btn-mini", tpe:="button", value:="Post Comment")
      )
    )
}
  
private [conversations] class ConversationGadget(conv:ConvNode, canComment:Boolean)(implicit val ecology:Ecology) 
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{
  /**
   * TODO: this is all wrong! It just does a straight flattening, but what we really want is much more
   * subtle, flattening only the primary nodes. We may, in fact, need to restructure the classes a bit to
   * make the tree make more sense: instead of having a ConversationGadget at all, we might have just
   * CommentGadget, and that recursively renders its replies.
   */
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
        replyContainer
      }
    )
      
  lazy val replyContainer = (new WrapperDiv)(cls:="_replyContainer offset1 span9").initialContent(replyPlaceholder)
  
  lazy val replyPlaceholder = 
    inp(cls:="_replyPlaceholder", 
      tpe:="text", 
      placeholder:="Click here to reply...",
      onclick:={ () => showRealReplyInput() },
      onkeydown:={ () => showRealReplyInput() })
  
  def showRealReplyInput():Unit = {
    replyContainer.replaceContents(realReply.rendered)
    realReply.focus()
  }
  
  lazy val realReply = new ReplyGadget("Reply here...")
}
