package querki.conversations

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.dom.{raw => dom}
import org.scalajs.jquery._
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import org.querki.facades.moment._

import querki.globals._

import querki.data.ThingInfo
import querki.display.{Gadget, QText, WrapperDiv}
import querki.display.input.DeleteButton
import querki.display.input.AutosizeFacade._

import messages._

class ConversationPane(val thingInfo:ThingInfo, focusedComment:Option[String])(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  
  implicit val t = thingInfo
  
  lazy val _onDisplay = Promise[ConversationPane]
  lazy val onDisplayFut:Future[ConversationPane] = _onDisplay.future
  
  override def onCreate(e:dom.HTMLDivElement) = {
    val fut = Client[ConversationFunctions].getConversationsFor(thingInfo.oid).call()
    // TODO: how can we encapsulate this error-catching universally for Client? This needs research:
    fut.onFailure {
      case t:Throwable => println(s"Got an error: $t")
    }
    fut.foreach { convInfo =>
      val convs =
        div(
          for (conv <- convInfo.convs)
            yield new ConversationGadget(conv, convInfo.canComment)
        )
      convWrapper.replaceContents(convs.render)
      
      val guts = 
        div(
          hr,
          h4(cls:="_commentsHeader", "Comments"),
          convWrapper,
          new ReplyGadget(None, "Start a new conversation...", { node => onNewConversation(node, convInfo.canComment) })
        )
        
      allWrapper.replaceContents(guts.render)
      InputGadgets.hookPendingGadgets()
      
      // If we're supposed to be focusing on a specific comment, show that:
      focusedComment.foreach { commentId =>
        val target = $(elem).find(s"a[name=$commentId]")
        // TODO: this highlight should probably fade out over, eg, three seconds?
        $(target).parent().addClass("_commentHighlight")
        // TODO: Eeeek! The signature for offset was more bad than usual. Fix this in the JQuery rewrite:
        $("html,body").scrollTop(target.offset().asInstanceOf[js.Dynamic].top.asInstanceOf[Int])        
      }
      
      _onDisplay.success(this)
    }
  }
  
  def onNewConversation(newNode:ConvNode, canComment:Boolean) = {
    val convGadget = new ConversationGadget(newNode, canComment)
    $(convWrapper.elem).append(convGadget.render)
  }
  
  lazy val convWrapper = new WrapperDiv
  lazy val allWrapper = new WrapperDiv
  
  def doRender() = div(allWrapper)
}

private [conversations] class CommentGadget(val comment:CommentInfo)(implicit val ecology:Ecology, thingInfo:ThingInfo)
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{  
  lazy val Client = interface[querki.client.Client]
  
  val cid = comment.id
  val created = moment(comment.createTime).calendar()
  
  def doRender() =
    div(
      cls:="_comment",
      id:=s"_comment$cid",
      a(cls:="_commentLink", name:=s"comment$cid"),
      if (comment.canDelete) {
        new DeleteButton(doDelete)
      },
      div(cls:="_commentHeader",
        span(cls:="_commentAuthor", comment.author.name),
        " ",
        span(cls:="_commentTime", created)
      ),
      new QText(comment.content, cls:="_commentText")
    )
    
  def doDelete() = {
    Client[ConversationFunctions].deleteComment(thingInfo.oid, cid).call().foreach { dummy =>
      $(elem).hide(400, { () => $(elem).remove() })
    }
  }
}

class ReplyGadget(replyTo:Option[CommentId], ph:String, onPosted:ConvNode => Unit)(implicit val ecology:Ecology, thingInfo:ThingInfo) 
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{
  lazy val Client = interface[querki.client.Client]
  
  override def onCreate(e:dom.HTMLDivElement) = {
    $(commentInput.elem).autosize()
  }
  
  def focus() = $(commentInput.elem).focus()
  
  def postComment():Unit = {
    Client[ConversationFunctions].addComment(thingInfo.oid, $(commentInput.elem).value().asInstanceOf[String], replyTo).call().foreach { node =>
      $(commentInput.elem).value("")
      onPosted(node)
    }
  }

  lazy val commentInput = Gadget(textarea(cls:="_commentInput", placeholder:=ph))
  
  def doRender() =
    div(cls:="_addComment row-fluid",
      div(cls:="span11",
        commentInput,
        inp(cls:="_postCommentButton btn btn-info btn-mini", 
          tpe:="button", 
          value:="Post Comment",
          onclick:={ () => postComment() })
      )
    )
}

private [conversations] class ConversationGadget(conv:ConvNode, canComment:Boolean)(implicit val ecology:Ecology, thingInfo:ThingInfo) 
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  
  /**
   * TODO: this is all wrong! It just does a straight flattening, but what we really want is much more
   * subtle, flattening only the primary nodes. We may, in fact, need to restructure the classes a bit to
   * make the tree make more sense: instead of having a ConversationGadget at all, we might have just
   * CommentGadget, and that recursively renders its replies.
   */
  def flattenNodes(node:ConvNode):Seq[CommentGadget] = {
    new CommentGadget(node.comment) +: node.responses.flatMap(flattenNodes(_))
  }
  lazy val flattenedNodes = flattenNodes(conv)
  
  lazy val commentContainer = Gadget(div(cls:="_commentContainer offset1 span9", flattenedNodes))
  
  def doRender() =
    div(
      cls:="_convThread row-fluid",
      commentContainer,
      if (canComment) {
        replyContainer
      }
    )
      
  lazy val replyContainer = (new WrapperDiv)(cls:="_replyContainer offset1 span9").initialContent(replyPlaceholder)
  
  lazy val replyPlaceholder = Gadget(
    inp(cls:="_replyPlaceholder", 
      tpe:="text", 
      placeholder:="Click here to reply...",
      onclick:={ () => showRealReplyInput() },
      onkeydown:={ () => showRealReplyInput() }))
  
  def showRealReplyInput():Unit = {
    replyContainer.replaceContents(realReply.rendered)
    realReply.focus()
  }
  
  lazy val realReply = new ReplyGadget(Some(flattenedNodes.last.comment.id), "Reply here...", onNewComment)
  
  def onNewComment(newNode:ConvNode) = {
    val gadgets = flattenNodes(newNode).map(_.rendered)
    $(commentContainer.elem).append(gadgets.toJSArray)
    replyContainer.replaceContents(replyPlaceholder.rendered)
    InputGadgets.hookPendingGadgets()
  }
}
