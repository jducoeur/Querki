package querki.local

import scala.util.Success

import org.querki.shocon._

import querki.comm._
import querki.globals._

/**
 * @author jducoeur
 */
class LocalizationEcot(e:Ecology) extends ClientEcot(e) with Localization {
  override def implements = Set(classOf[Localization])
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  val _readyPromise = Promise[Unit]
  val ready = _readyPromise.future
  
  case class MessagesImpl(name:String, msgs:ObjectValue) extends Messages {
    def getPackage(pkgName:String):Messages = {
      msgs.vs.get(pkgName) match {
        case Some(child @ ObjectValue(_)) => MessagesImpl(pkgName, child)
        case _ => throw new Exception(s"In Messages package $name, failed to find subpackage $pkgName")
      }
    }
    
    def msg(msgName:String, params:(String, String)*):String = {
      // TODO: deal with params if present
      msgs.vs.get(msgName) match {
        case Some(child @ SimpleValue(text)) => text
        case _ => throw new Exception(s"In Messages package $name, failed to find message $msgName")
      }
    }
  }
  
  var _messages:Option[Messages] = None
  def allMessages = _messages.get
  def messages(name:String) = allMessages.getPackage(name)
  
  override def postInit() = {
    val ajaxCall:PlayAjax = controllers.Assets.at("messages/default/clientStrings")
    ajaxCall.callAjax().map { messageText =>
      val hoconTable = HoconParse(messageText)
      _messages = Some(MessagesImpl("", hoconTable))
      _readyPromise.complete(Success())
    }
  }
}
