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
    val state:Option[SpaceState], 
    val thing:Option[Thing],
    val ecology:Ecology,
    val numNotifications:Int = 0) extends EcologyMember
{
  lazy val Person = interface[querki.identity.Person]
  
  def requesterOrAnon = requester getOrElse User.Anonymous
  def requesterOID = requester map (_.id) getOrElse UnknownOID  
  def ownerHandle = state.map(_.ownerHandle).getOrElse(ownerId.toThingId.toString)
  def ownerName = state.map(_.ownerName).getOrElse(ownerId.toThingId.toString)
  
  def withUpdatedState(newState:SpaceState):RequestContext
  
  def isOwner = requesterOrAnon.hasIdentity(ownerId)
  
  /**
   * Fetch the value of the named parameter of this request.
   * 
   * TBD: this is an HTTP-ism bleeding into the abstraction. Is it appropriate? It's awfully *convenient*,
   * but we'll see whether it's right.
   */
  def queryParam(paramName:String):Seq[String]
  
  /**
   * The UIRenderer to use for displaying stuff to the user in this context.
   */
  def renderer:UIRenderer
  
  /**
   * The identity that is making this request.
   * 
   * TODO: we need a much, much better concept of "the Identity that I am using within this Space", if we're
   * going to truly support Identity separation properly.
   */
  def localIdentity:Option[Identity] = {
    for {
      req <- requester
      s <- state
      firstIdentity <- Person.localIdentities(req)(s).headOption
    }
      yield firstIdentity
  }
  
  /**
   * Replace the State and return the modified RequestContext. Basically a limited copy() function.
   */
  def +(state:SpaceState):RequestContext
  
  /**
   * Replace the State and return the modified RequestContext. Basically a limited copy() function.
   */
  def +(thing:Thing):RequestContext
}
