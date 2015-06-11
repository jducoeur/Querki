package querki.values

import models.OID
import models.UnknownOID
import models.Thing

import querki.ecology._

import querki.identity.{Identity, User}

import querki.ui.UIRenderer

/**
 * As Querki gets more complex, we're passing larger and larger bundles of information around.
 * So instead of trying to do that all in separate parameters, we're taking all the common
 * parts and building up this RequestContext object.
 * 
 * This initially gets filled by the request itself; as things process, this may get replaced
 * by more-filled versions.
 * 
 * This object is a bit squishy semantically, but the high concept is that it should encapsulate
 * all the data that we *typically* pass around into *most* page renders.
 * 
 * Note that this is an abstract class. You will typically pass subclasses around. RequestContext
 * represents the abstract *concept* of a request; subclasses represent the actual ways this
 * happens.
 * 
 * @param requester The validated User who is asking for this page, if any.
 */
abstract class RequestContext(
    val requester:Option[User], 
    // Note that this is an *identity*
    val ownerId:OID, 
    val ecology:Ecology,
    val numNotifications:Int = 0) extends EcologyMember
{
  lazy val Person = interface[querki.identity.Person]
  
  def requesterOrAnon = requester getOrElse User.Anonymous
  def requesterOID = requester map (_.id) getOrElse UnknownOID  
  def ownerHandle(implicit state:SpaceState) = state.ownerHandle
  def ownerName(implicit state:SpaceState) = state.ownerName
  
  def isOwner = requesterOrAnon.hasIdentity(ownerId)
  
  /**
   * Fetch the value of the named parameter of this request.
   * 
   * TBD: this is an HTTP-ism bleeding into the abstraction. Is it appropriate? It's awfully *convenient*,
   * but we'll see whether it's right.
   */
  def queryParam(paramName:String):Seq[String]
  
  /**
   * The identity that is making this request.
   * 
   * TODO: we need a much, much better concept of "the Identity that I am using within this Space", if we're
   * going to truly support Identity separation properly.
   */
  def localIdentity(implicit state:SpaceState):Option[Identity] = {
    for {
      req <- requester
      firstIdentity <- Person.localIdentities(req)(state).headOption
    }
      yield firstIdentity
  }
}
