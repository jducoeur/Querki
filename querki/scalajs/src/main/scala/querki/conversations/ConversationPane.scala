package querki.conversations

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.data.ThingInfo
import querki.display.{Gadget, WrapperDiv}

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
      val guts = div(hr, p(s"Conversations will go here: there are ${convInfo.convs.length} of them"))
      convWrapper.replaceContents(guts.render)
      InputGadgets.hookPendingGadgets()
    }
  }
  
  lazy val convWrapper = new WrapperDiv
  
  def doRender() = div(convWrapper)
}
