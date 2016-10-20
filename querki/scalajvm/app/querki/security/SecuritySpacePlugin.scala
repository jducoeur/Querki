package querki.security

import akka.actor.Actor

import models.{Kind, Thing}
import querki.globals._
import querki.spaces._
import querki.spaces.messages._
import querki.util.UnexpectedPublicException
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
   * This will be called during the Actor's receive loop, and provides supplementary
   * handlers particular to Apps.
   */
  def receive:Actor.Receive = {
    case SpacePluginMsg(req, _, GetInstancePermissionsObject(thingId)) => {
      val state = api.state
      state.anything(thingId) match {
        case Some(thing) => {
          val hasInstancePerms = (thing.kind == Kind.Space || thing.isModel(state))
          if (hasInstancePerms) {
            val permThingOpt = for {
              oid <- thing.getFirstOpt(AccessControl.InstancePermissionsProp)(state)
              t <- state.anything(oid)
            }
              yield t
        
            // Either we have the Instance Permissions Thing, or we create it:
            val permThingRM:RM[(Thing, SpaceState)] = permThingOpt match {
              case Some(t) => rtc.successful((t, state))
              case _ => {
                val permProps = Map(Basic.DisplayNameProp(s"__${thing.displayName} Instance Permissions"))
                for {
                  // Create the Permissions Thing:
                  result <- api.doCreate(req, MOIDs.InstancePermissionsModelOID, permProps, Kind.Thing, false)
                  (newState, permThingId) = result
                  permThing = newState.anything(permThingId).get
                  // And point the main Thing to it:
                  fullState <- api.modifyThing(req, thingId, None, Map(AccessControl.InstancePermissionsProp(permThingId)), false, false)
                }
                  yield (permThing, fullState)
              }
            }
              
            permThingRM.map { case (permThing, fullState) =>
              api.respond(ThingFound(permThing.id, fullState))
            }
          } else {
            // This doesn't have Instance Permissions, so just toss it right back:
            api.respond(ThingFound(thingId, state))
          }
        }
        case _ => api.respond(ThingError(UnexpectedPublicException))
      }
    }
  }
}

/**
 * Fetches the ThingPermissions object for the specified Thing, creating them if need be.
 * Expects a ThingFound for the permissions object.
 */
private [security] case class GetInstancePermissionsObject(thingId:OID)
