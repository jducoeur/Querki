package querki.history

import models.{AsOID, ThingId}

import querki.api._
import querki.data.{SpaceInfo, ThingInfo, TID}
import querki.globals._
import querki.spaces.messages.ThingFound

class HistoryFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with HistoryFunctions {
  import HistoryFunctions._
  import SpaceHistory._
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ClientApi = interface[querki.api.ClientApi]
  
  def doRoute(req:Request):Future[String] = route[HistoryFunctions](this)(req)
  
  // At least for the time being, we are considering History management to fall under more-general Data management.
  // We might make this more fine-grained eventually (especially for management of specific Things), but Managers
  // should always be able to do it.
  lazy val canManageHistory = AccessControl.hasPermission(AccessControl.CanManageDataPerm, state, user, state)

  def getHistorySummary():Future[HistorySummary] = {
    // Only Managers can call this:
    if (!canManageHistory)
      throw new Exception(s"Only the owner of the Space is allowed to view its history!")
    
    for {
      summary <- spaceRouter.requestFor[HistorySummary](GetHistorySummary(rc))
    }
      yield summary
  }
  
  def rollbackTo(v:HistoryVersion):Future[SpaceInfo] = {
    if (!canManageHistory)
      throw new Exception(s"Only the owner of the Space is allowed to view its history!")
    
    for {
      ThingFound(id, newState) <- spaceRouter.request(RollbackTo(v, user))
    }
      yield ClientApi.spaceInfo(newState, user)
  }
  
  def restoreDeletedThing(tid:TID):Future[ThingInfo] = {
    // TODO: this abstraction is leaking all over the place. Can we come up with a better-typed way of saying
    // that this parameter is specifically an OID, and converting it as such?
    ThingId(tid.underlying) match {
      case AsOID(oid) => {
        for {
          ThingFound(id, newState) <- spaceRouter.request(RestoreDeletedThing(rc, oid))
          restored = newState.anything(id).getOrElse(throw new Exception(s"Couldn't find Thing $tid!"))
          result <- ClientApi.thingInfo(restored, rc)(newState)
        }
          yield result
      }
      case _ => throw new Exception(s"restoreDeletedThing currently only works with OIDs!")
    }
  }
}
