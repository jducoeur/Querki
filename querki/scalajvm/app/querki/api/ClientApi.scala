package querki.api

import upickle._

import models.AsOID

import querki.global._

import querki.data.{IdentityInfo, RequestInfo, SpaceInfo, ThingInfo, UserInfo}
import querki.identity.User
import querki.values.RequestContext

class ClientApiEcot(e:Ecology) extends QuerkiEcot(e) with ClientApi {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  
  def thingInfo(topt:Option[Thing], rc:RequestContext):Option[ThingInfo] = {
    topt.map { t => 
      implicit val state = rc.state.get
      val user = rc.requesterOrAnon
      val editable = AccessControl.canEdit(state, user, t.id)
      ThingInfo(
        AsOID(t.id), 
        t.linkName, 
        t.unsafeDisplayName,
        t.kind,
        editable,
        editable && DataModelAccess.isDeletable(t))
    }
  }
  
  def spaceInfo(topt:Option[SpaceState], rc:RequestContext):Option[SpaceInfo] = {
    topt.map { t => 
      SpaceInfo(
        AsOID(t.id), 
        t.linkName, 
        t.unsafeDisplayName,
        t.ownerHandle)
    }
  }
  
  def userInfo(uopt:Option[User]):Option[UserInfo] = {
    uopt.map { user =>
      // TODO: this will need adjusting when we have multiple Identities. The mainIdentity should come first:
      val identityInfos = user.identities.map { identity =>
        IdentityInfo(AsOID(identity.id), identity.name, identity.handle)
      }
      UserInfo(AsOID(user.id), identityInfos)
    }
  }
  
  def requestInfo(rc:RequestContext):RequestInfo =
      RequestInfo(
        userInfo(rc.requester), 
        spaceInfo(rc.state, rc), 
        thingInfo(rc.thing, rc),
        rc.isOwner,
        rc.requesterOrAnon.isAdmin)
  
  def pickleRequest(rc:RequestContext):String = {
    write(requestInfo(rc))
  }
}
