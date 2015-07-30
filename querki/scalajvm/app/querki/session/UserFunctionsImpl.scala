package querki.session

import scala.concurrent.Future

import autowire._

import querki.api.{AutowireApiImpl, AutowireParams}
import querki.data.{TID, SpaceInfo}
import querki.globals._
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
}
