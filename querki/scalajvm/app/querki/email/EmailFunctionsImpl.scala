package querki.email

import querki.api._
import querki.globals._

import EmailFunctions._

class EmailFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with EmailFunctions {
  
  lazy val Email = interface[Email]
  lazy val Unsubscribe = interface[Unsubscribe]
  
  def doRoute(req:Request):Future[String] = route[EmailFunctions](this)(req)
 
  def getUnsubOptionsFor(unsubStr:String):Future[UnsubPageInfo] = {
    val unsubInfo = Unsubscribe.parseUnsub(unsubStr).getOrElse(throw new Exception(s"Failed to parse unsubStr $unsubStr"))
    if (!rc.requesterOrAnon.hasIdentity(unsubInfo.user.mainIdentity.id))
      throw new Exception(s"Suspicious attempt by user ${rc.requesterOrAnon} to use unsub link $unsubStr with parsed User ${unsubInfo.user}!")
    
    val notifier = Email.emailNotifier(unsubInfo.notifier)
    notifier.unsubOptions(unsubInfo).map { case (emailInfo, unsubOptions) =>
      UnsubPageInfo(unsubInfo.user.mainIdentity.id.toTOID, emailInfo, unsubOptions)
    }
  }
}
