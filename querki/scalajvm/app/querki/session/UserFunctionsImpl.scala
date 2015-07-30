package querki.session

import scala.concurrent.Future

import autowire._

import models.ThingId

import querki.api.{AutowireApiImpl, AutowireParams}
import querki.data.{TID, SpaceInfo}
import querki.globals._
import querki.identity.UserLevel
import querki.spaces.messages._

/**
 * Implementation of the UserFunctions API. This basically covers most of the non-Space
 * stuff, that you can do without being *in* a Space.
 * 
 * @author jducoeur
 */
class UserFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with UserFunctions {
  import UserFunctions._
  
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  lazy val spaceManager = SpaceOps.spaceManager
  
  def doRoute(req:Request):Future[String] = route[UserFunctions](this)(req)
  
  implicit def spaceDetails2Info(details:SpaceDetails):SpaceInfo = {
    SpaceInfo(TID(details.id.toThingId), Some(details.handle), details.display, "", details.ownerHandle)
  }
  
  def listSpaces():Future[AllSpaces] = {
    spaceManager.requestFor[MySpaces](ListMySpaces(user.id)) map { mySpaces =>
      AllSpaces(mySpaces.ownedByMe.map(spaceDetails2Info), mySpaces.memberOf.map(spaceDetails2Info))
    }
  }
  
  def accountInfo():Future[AccountInfo] = {
    // TODO: this is doing a direct DB access, which is by definition ugly.
    val pairOpt = UserAccess.getIdentity(ThingId(user.mainIdentity.handle))
    pairOpt match {
      case Some((identity, level)) => {
        Future.successful(AccountInfo(identity.handle, identity.name, identity.email.addr, level))
      }
      case None => throw new Exception(s"Somehow tried to get the accountInfo for unknown user ${user.mainIdentity.handle}!")
    }    
  }
}
