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
    println("Created the ConversationPane")
    val fut = Client[ConversationFunctions].getConversationsFor(thingInfo.oid).call()
    // TODO: how can we encapsulate this error-catching universally for Client? This needs research:
    fut.onFailure {
      case t:Throwable => println(s"Got an error: $t")
    }
    fut.foreach { convInfo =>
      println(s"Got the confInfo: $convInfo")
      val guts = div(hr, p("Conversations will go here"))
      convWrapper.replaceContents(guts.render)
      InputGadgets.hookPendingGadgets()
    }
  }
  
  lazy val convWrapper = new WrapperDiv
  
  def doRender() = div(convWrapper)
}
