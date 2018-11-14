package querki.conversations

import org.scalajs.dom.{raw => dom}
import dom.Element
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import models.HtmlWikitext
import querki.data.{IdentityInfo, TID}
import querki.display.{HookedGadget, QText}
import querki.display.input.DeleteButton
import querki.globals._
import querki.time._

private [conversations] class CommentGadget(val comment:CommentInfo, val thingId:TID)(implicit val ecology:Ecology)
  extends Gadget[dom.HTMLDivElement] with EcologyMember 
{  
  lazy val Client = interface[querki.client.Client]
  
  val cid = comment.id
  val created = displayTime(comment.createTime)
  
  def doRender() = {
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
        span(cls:="_commentTime", s"(@${comment.author.handle}) $created")
      ),
      new QText(comment.content, cls:="_commentText")
    )
  }
    
  def doDelete() = {
    Client[ConversationFunctions].deleteComment(thingId, cid).call().foreach { dummy =>
      $(elem).hide(400, { () => $(elem).remove() })
    }
  }
}

object CommentGadget {
  /**
   * Given an Element that describes a Comment, this extracts the info from that.
   */
  def infoFromElem(e:Element)(implicit ecology:Ecology):CommentInfo = {
    CommentInfo(
      $(e).data("commentid").get.asInstanceOf[Integer],
      IdentityInfo(
        $(e).dataString("authorid"),
        $(e).dataString("authorname"),
        // TODO: this isn't right; can we fetch the actual handle without pain server-side?
        $(e).dataString("authorid")),
      HtmlWikitext($(e).html()),
      // TODO: we will eventually need machinery to cope with sub-threads:
      true,
      $(e).data("time").get.asInstanceOf[Double].toLong,
      // TODO: we should eventually understand how deletion works in this world:
      false,
      false
    )
  }
  
  /**
   * Given an Element that describes a Comment, replace it with the UI version.
   * 
   * This is horrible and side-effecting! We should come up with a less fugly
   * abstraction that works with Gadgets.
   */
  def fromElem(e:Element)(implicit ecology:Ecology):CommentGadget = {
    val info = infoFromElem(e)
    val gadget = new CommentGadget(info, TID($(e).dataString("thingid")))
    $(e).replaceWith(gadget.rendered)
    gadget
  }
}
