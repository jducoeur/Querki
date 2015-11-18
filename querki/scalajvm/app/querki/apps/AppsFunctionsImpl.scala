package querki.apps

import models.{AsOID, ThingId}

import querki.api.{AutowireParams, SpaceApiImpl}
import querki.data.{SpaceInfo, TID}
import querki.globals._
import querki.spaces.messages._

/**
 * @author jducoeur
 */
class AppsFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with AppsFunctions  {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[Apps]
  lazy val AppsInternal = interface[AppsInternal]
  lazy val ClientApi = interface[querki.api.ClientApi]
  
  def doRoute(req:Request):Future[String] = route[AppsFunctions](this)(req)
  
  def getApps():Seq[SpaceInfo] = {
    for {
      app <- state.apps
    }
      yield ClientApi.spaceInfo(app)
  }
  
  def addApp(appIdStr:String):Future[Unit] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    ThingId(appIdStr) match {
      case AsOID(appId) => {
        (spaceRouter ? SpacePluginMsg(user, state.id, AddApp(appId))) map {
          case ThingFound(_, _) => ()
          case ThingError(ex, _) => throw ex
        }
      }
      case _ => throw new PublicException("Apps.notASpace")
    }
  }
  
  def extractApp(elements:Seq[TID]):Future[Unit] = {
    // TEMP:
    QLog.spew("In extractApp:")
    elements foreach { elementId =>
      state.anything(ThingId(elementId.underlying)) match {
        case Some(element) => QLog.spewThing(element)(state)
        case _ => QLog.error(s"Unknown TID $elementId!")
      }
    }
    
    // TODO: return a progress connection
    Future.successful(())
  }
}
