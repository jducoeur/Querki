package querki.api

import upickle._

import models.AsOID

import querki.globals._

import querki.core.NameUtils
import querki.data.{IdentityInfo, RequestInfo, SpaceInfo, ThingInfo, UserInfo}
import querki.identity.{PublicIdentity, User}
import querki.pages.PageDetails
import querki.tags.IsTag
import querki.values.RequestContext

class ClientApiEcot(e:Ecology) extends QuerkiEcot(e) with ClientApi {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  
  def thingInfo(t:Thing, rc:RequestContext):ThingInfo = {
      implicit val state = rc.state.get
      val user = rc.requesterOrAnon
      val editable = AccessControl.canEdit(state, user, t.id)
      val isModel = t.isModel
      ThingInfo(
        AsOID(t.id), 
        t.linkName, 
        t.unsafeDisplayName,
        AsOID(t.model),
        t.kind,
        isModel,
        editable,
        editable && DataModelAccess.isDeletable(t),
        isModel && AccessControl.canCreate(state, user, t),
        t.isInstanceOf[IsTag])
  }
  
  def spaceInfo(topt:Option[SpaceState], rc:RequestContext):Option[SpaceInfo] = {
    topt.map { t => 
      SpaceInfo(
        AsOID(t.id), 
        // TODO: NameUtils.toUrl() is inconsistent with SafeUrl: they handle spaces differently.
        // We need to fix this inconsistency!
        t.linkName.map(NameUtils.toUrl(_)), 
        t.unsafeDisplayName,
        t.owner.toThingId.toString,
        t.ownerHandle)
    }
  }
  
  def identityInfo(identity:PublicIdentity):IdentityInfo = {
    IdentityInfo(AsOID(identity.id), identity.name, identity.handle)
  }
  
  def userInfo(uopt:Option[User]):Option[UserInfo] = {
    uopt.map { user =>
      // TODO: this will need adjusting when we have multiple Identities. The mainIdentity should come first:
      val identityInfos = user.identities.map { identity =>
        identityInfo(identity)
      }
      UserInfo(AsOID(user.id), identityInfos)
    }
  }
  
  def requestInfo(rc:RequestContext):RequestInfo = {
    RequestInfo(
      userInfo(rc.requester), 
      spaceInfo(rc.state, rc), 
      rc.isOwner,
      rc.requesterOrAnon.isAdmin)
  }
}
