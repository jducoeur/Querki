package querki.conversations

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import querki.data.ThingInfo
import querki.display.QText
import querki.display.input.DeleteButton
import querki.globals._
import querki.time._

private [conversations] class CommentGadget(val comment:CommentInfo)(implicit val ecology:Ecology, thingInfo:ThingInfo)
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{  
  lazy val Client = interface[querki.client.Client]
  
  val cid = comment.id
  val created = displayTime(comment.createTime)
  
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
