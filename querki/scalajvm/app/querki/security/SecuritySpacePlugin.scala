package querki.security

import akka.actor.Actor

import models.{Kind, Thing}
import querki.globals._
import querki.identity.User
import querki.spaces._
import querki.spaces.messages._
import querki.util.PublicException
import querki.values.QValue

import SecurityFunctions._

/**
 * This code runs *inside* the Space Actor, as a plugin. It has important constraints, as described in
 * SpaceAPI.
 * 
 * @author jducoeur
 */
class SecuritySpacePlugin[RM[_]](api:SpaceAPI[RM], rtc:RTCAble[RM], implicit val ecology:Ecology) 
  extends SpacePlugin(api, rtc) with EcologyMember
{
  lazy val AccessControl = interface[AccessControl]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  
  implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  /**
   * Fetches the Instance Permissions for the specified Thing, if that makes sense and creating
   * it if necessary.
   */
  def getInstancePermissionsThing(req:User, thing:Thing)(state:SpaceState):RM[ChangeResult] = {
    val thingId = thing.id
    val hasInstancePerms = (thing.kind == Kind.Space || thing.isModel(state))
    if (hasInstancePerms) {
      val permThingOpt = for {
        oid <- thing.getFirstOpt(AccessControl.InstancePermissionsProp)(state)
        t <- state.anything(oid)
      }
        yield t
  
      // Either we have the Instance Permissions Thing, or we create it:
      permThingOpt match {
        case Some(t) => rtc.successful(ChangeResult(List.empty, Some(t.id), state))
        case _ => {
          val permProps = Map(Basic.DisplayNameProp(s"__${thing.displayName} Instance Permissions"))
          for {
            // Create the Permissions Thing:
            createResult <- api.doCreate(req, MOIDs.InstancePermissionsModelOID, permProps, Kind.Thing)(state)
            ChangeResult(createEvents, permThingIdOpt, newState) = createResult
            permThingId = permThingIdOpt.get
            permThing = newState.anything(permThingId).get
            // And point the main Thing to it:
            modifyResult <- api.modifyThing(req, thing.id, None, Map(AccessControl.InstancePermissionsProp(permThingId)), false)(newState)
            ChangeResult(modifyEvents, _, fullState) = modifyResult
            // Okay, everything seems to check out. Persist the whole thing atomically:
            _ <- api.persistAllAnd(createEvents ++ modifyEvents)
          }
            yield ChangeResult(createEvents ++ modifyEvents, Some(permThingId), fullState) 
        }
      }
    } else {
      // This doesn't have Instance Permissions, so just toss it right back:
      rtc.successful(ChangeResult(List.empty, Some(thingId), state))
    }    
  }
  
  /**
   * This will be called during the Actor's receive loop, and provides supplementary
   * handlers particular to Apps.
   */
  def receive:Actor.Receive = {
    case SpacePluginMsg(req, _, GetInstancePermissionsObject(thingId)) => {
      val state = api.currentState
      state.anything(thingId) match {
        case Some(thing) => api.runAndSendResponse("getInstancePermissions", getInstancePermissionsThing(req, thing))(state)
        case _ => api.respond(ThingError(PublicException("Thing.find.noSuch")))
      }
    }
  }
}

/**
 * Fetches the ThingPermissions object for the specified Thing, creating them if need be.
 * Expects a ThingFound for the permissions object.
 */
private [security] case class GetInstancePermissionsObject(thingId:OID)
