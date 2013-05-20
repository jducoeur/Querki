package querki.access

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import modules.Modules._

import identity.User

/**
 * This is the public API for ACL management. The rest of Querki should work through here.
 */
object AccessControl {
  import modules.Modules.AccessControl._
  
  // TODO: this needs to become *much* more complete, including being able to manage access control on
  // a thing-by-thing basis, and caching the results, not to mention being a decent system. For the
  // moment, this is simply hacked, to allow space-specific-users the ability to edit:
  def hasPermission(aclProp:Property[QLText,_], state:SpaceState, who:User, thingId:OID):Boolean = {
    who match {
      // HACK: just to allow invitees to edit RSVPs in the Wedding site:
      case modules.Modules.Person.SpaceSpecificUser(_, _, _, spaceId) => (spaceId == state.id)
      case _ => false
    }
  }
  
  def canEdit(state:SpaceState, who:User, thingId:OID):Boolean = {
    (who.id == state.owner) || (hasPermission(canEditProp, state, who, thingId))
  }
}

class AccessControlModule(val moduleId:Short) extends modules.Module {

  object MOIDs {
    val CanEditOID = moid(1)
  }
  import MOIDs._
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val canEditProp = new SystemProperty(CanEditOID, QLType, Optional,
      toProps(
        setName("Can Edit"),
        DisplayTextProp("This property says who else can edit this Thing")))
  
  override lazy val props = Seq(
    canEditProp
  )
}