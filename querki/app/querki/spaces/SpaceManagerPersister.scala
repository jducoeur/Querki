package querki.spaces

import akka.actor._

import anorm.{Success=>AnormSuccess,_}
import play.api.db._
import play.api.Play.current

import messages._

import models.{AsName, OID}
import models.system.SystemSpace
import models.system.OIDs.{systemOID, SystemUserOID}

import querki.db.ShardKind
import ShardKind._

/**
 * This Actor deals with all database-y stuff for the SpaceManager. In practice,
 * this means that most of the actions that SpaceManager *itself* does, such as looking
 * up, listing or creating Spaces, happens in here, while the SpaceManager itself mostly
 * does routing to the Spaces. This is appropriate, since SpaceManager is a key bottleneck,
 * so we want to keep it fast and high-reliability.
 * 
 * IMPORTANT: we create a *pool* of SpaceManagerPersisters to work with the SpaceManager.
 * This means that it is very important to keep this class stateless! You have no way of
 * knowing, in principle, which persister any given message will go to.
 */
private [spaces] class SpaceManagerPersister extends Actor {

  def receive = {
    case ListMySpaces(owner) => {
        // TODO: this involves DB access, so should be async using the Actor DSL
        // Note that Spaces are now indexed by Identity, so we need to indirect
        // through that table. (Since our parameter is User OID.)
        val mySpaces = DB.withConnection(dbName(System)) { implicit conn =>
          val spaceStream = SQL("""
              SELECT Spaces.id, Spaces.name, display, Identity.handle FROM Spaces
                JOIN Identity ON userid={owner}
              WHERE owner = Identity.id
              """).on("owner" -> owner.raw)()
          spaceStream.force.map { row =>
            val id = OID(row.get[Long]("id").get)
            val name = row.get[String]("name").get
            val display = row.get[String]("display").get
            // TODO: this is clearly wrong -- eventually, we will allow handle to be NULL. So we need to also
            // fetch Identity.id, and ThingId that if we don't find a handle:
            val ownerHandle = row.get[String]("handle").get
            SpaceDetails(AsName(name), id, display, AsName(ownerHandle))
          }
        } ++ { if (owner == SystemUserOID) { Seq(SpaceDetails(AsName("System"), systemOID, SystemSpace.State.name, AsName("systemUser"))) } else Seq.empty }
        val memberOf = DB.withConnection(dbName(System)) { implicit conn =>
          // Note that this gets a bit convoluted, by necessity. We are coming in through a User;
          // translating that to Identities; getting all of the Spaces that those Identities are members of;
          // then getting the Identities that own those Spaces so we can get their handles. Hence the
          // need to alias the Identity table.
          val spaceStream = SQL("""
              SELECT Spaces.id, Spaces.name, display, OwnerIdentity.handle FROM Spaces
                JOIN Identity AS RequesterIdentity ON userid={owner}
                JOIN SpaceMembership ON identityId=RequesterIdentity.id
                JOIN Identity AS OwnerIdentity ON OwnerIdentity.id=Spaces.owner
              WHERE Spaces.id = SpaceMembership.spaceId
              """).on("owner" -> owner.raw)()
          spaceStream.force.map { row =>
            val id = OID(row.get[Long]("id").get)
            val name = row.get[String]("name").get
            val display = row.get[String]("display").get
            // TODO: this is clearly wrong -- eventually, we will allow handle to be NULL. So we need to also
            // fetch Identity.id, and ThingId that if we don't find a handle:
            val ownerHandle = row.get[String]("handle").get
            SpaceDetails(AsName(name), id, display, AsName(ownerHandle))
          }
        }
        sender ! MySpaces(mySpaces, memberOf)
    }
    
  }
}