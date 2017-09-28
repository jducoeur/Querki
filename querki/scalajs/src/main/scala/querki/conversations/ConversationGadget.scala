package querki.conversations

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all.{input => inp, _}

import org.querki.gadgets._
import org.querki.jquery._
import org.querki.squery.Focusable._

import querki.data.ThingInfo
import querki.display.WrapperDiv
import querki.globals._

private [conversations] class ConversationGadget(conv:ConvNode, canComment:Boolean)(implicit val ecology:Ecology, thingInfo:ThingInfo) 
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  /**
   * TODO: this is all wrong! It just does a straight flattening, but what we really want is much more
   * subtle, flattening only the primary nodes. We may, in fact, need to restructure the classes a bit to
   * make the tree make more sense: instead of having a ConversationGadget at all, we might have just
   * CommentGadget, and that recursively renders its replies.
   */
  def flattenNodes(node:ConvNode):Seq[CommentGadget] = {
    new CommentGadget(node.comment, thingInfo.oid) +: node.responses.flatMap(flattenNodes(_))
  }
  lazy val flattenedNodes = flattenNodes(conv)
  
  lazy val commentContainer = Gadget(div(cls:="_commentContainer col-md-offset1 col-md-9", flattenedNodes))
  
  def doRender() =
    div(
      cls:="_convThread row",
      commentContainer,
      if (canComment) {
        replyContainer
      }
    )
      
  lazy val replyContainer = (new WrapperDiv()(ecology))(cls:="_replyContainer col-md-offset1 col-md-9").initialContent(replyPlaceholder)
  
  lazy val replyPlaceholder = Gadget(
    inp(cls:="_replyPlaceholder form-control", 
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
    $(commentContainer.elem).append(gadgets)
    replyContainer.replaceContents(replyPlaceholder.rendered)
    Gadgets.hookPendingGadgets()
  }
}
