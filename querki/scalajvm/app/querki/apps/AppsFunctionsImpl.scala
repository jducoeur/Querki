package querki.apps

import models.{AsOID, ThingId}

import querki.api.{AutowireParams, OperationHandle, ProgressActor, SpaceApiImpl}
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
  
  def extractApp(elements:Seq[TID], name:String):OperationHandle = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    // IMPORTANT: the ExtractAppActor is created at the top level, *not* as part of the current
    // troupe! This is necessary because it will reboot the troupe a couple of times in the course
    // of extraction. (We don't put it under the user's context, because we'd like to start on the
    // same node as the Space, to avoid having to send the State across the wire.)
    ProgressActor.createProgressActor(requester, ExtractAppActor.props(ecology, elements, name, user, state), true)
  }
}
