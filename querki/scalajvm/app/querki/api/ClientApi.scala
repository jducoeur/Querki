package querki.api

import scala.concurrent.Future

import upickle._
import autowire._

import models.{AsOID, HtmlWikitext}

import querki.globals._
import querki.globals.Implicits._

import querki.core.NameUtils
import querki.data._
import querki.identity.{PublicIdentity, User}
import querki.pages.PageDetails
import querki.session.messages.{ClientAnswer, ClientResponse}
import querki.tags.IsTag
import querki.types.ModelTypeBase
import querki.values.{QLRequestContext, RequestContext}

class ClientApiEcot(e:Ecology) extends QuerkiEcot(e) with ClientApi with CommonFunctionsImpl
  with autowire.Server[String, upickle.Reader, upickle.Writer]
{
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  lazy val Editor = interface[querki.editing.Editor]
  
  def thingInfo(t:Thing, rc:RequestContext):ThingInfo = {
      implicit val state = rc.state.get
      val user = rc.requesterOrAnon
      val editable = AccessControl.canEdit(state, user, t.id)
      val isModel = t.isModel
      val importedFrom =
        if (t.spaceId == state.id)
          None
        else
          spaceInfo(state.getApp(t.spaceId), rc)
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
        t.isInstanceOf[IsTag],
        importedFrom)
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
  
  def propInfo(prop:AnyProp, rc:RequestContext):PropInfo = {
    implicit val s = rc.state.get
    val typeId = prop.pType match {
      case mt:ModelTypeBase => {
        if (prop.pType.ifSet(Basic.ExplicitProp))
          prop.pType.id
        else
          mt.basedOn
      }
      case _ => prop.pType.id
    }
    PropInfo(
      prop.id.toThingId, 
      prop.linkName, 
      prop.displayName, 
      prop.getPropOpt(Core.AppliesToKindProp).flatMap(_.firstOpt),
      prop.cType.id.toThingId,
      typeId.toThingId)
  }
  
  def propValInfo(t:Thing, rc:RequestContext):Seq[PropValInfo] = {
    implicit val state = rc.state.get
    def oneProp(prop:AnyProp, v:QValue):PropValInfo = {
      val prompt = prop.getPropOpt(Editor.PromptProp).map(_.renderPlain)
      val renderedV =
        if (v.pType.isInstanceOf[querki.core.IsTextType]) {
          HtmlWikitext(s"<pre><code>${v.cv.map(v.pType.toUser(_)).mkString("\n")}</code></pre>")
        } else {
          v.wikify(QLRequestContext(rc))
      }
      val tooltip = prop.getPropOpt(Conventions.PropSummary).map(_.render(prop.thisAsContext(rc)))
          
      PropValInfo(propInfo(prop, rc), prompt, renderedV, tooltip)
    }
    
    val infoOpts = for {
      prop <- t.localProps
      if (!prop.ifSet(Core.InternalProp))
    }
      yield t.getPropOpt(prop).map(pv => oneProp(prop, pv.v))
    infoOpts.flatten.toSeq
  }
  
  // Autowire functions
  def write[Result: Writer](r: Result) = upickle.write(r)
  def read[Result: Reader](p: String) = upickle.read[Result](p)
  
  def handleCommonFunction(req:autowire.Core.Request[String]):Future[ClientAnswer] = {
    route[CommonFunctions](this)(req).map(ClientResponse(_))
  }
}
