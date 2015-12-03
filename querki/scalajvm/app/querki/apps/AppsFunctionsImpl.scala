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
  lazy val ClientApi = interface[querki.api.ClientApi]
  
  def doRoute(req:Request):Future[String] = route[AppsFunctions](this)(req)
  
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
    
    // TBD: for now, we're creating the extraction Actor here, within the Space's troupe. I *think* this
    // works, because Space.reload() leaves the Actor hierarchy in place and just reloads the data. This
    // needs sanity-checking, though, and we should keep an eye on whether it remains true once we move
    // to the Akka Persistence mechanism. If it doesn't work, then switch the last parameter here to "true",
    // to make the extraction Actor top-level.
    ProgressActor.createProgressActor(requester, ExtractAppActor.props(ecology, elements, name, user, state, spaceRouter), false)
  }
}
