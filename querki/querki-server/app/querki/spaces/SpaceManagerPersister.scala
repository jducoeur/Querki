package querki.spaces

import scala.util._

import akka.actor._

import anorm.{Success=>AnormSuccess,_}
import play.api.db._
import play.api.Play.current

import com.github.nscala_time.time.Imports._

import messages._

import models.{AsName, OID}
import models.{Kind, Thing}

import querki.db.ShardKind
import ShardKind._

import querki.ecology._
import querki.identity.MembershipState
import querki.util._
import querki.util.SqlHelpers._

import PersistMessages._

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
 * 
 * TODO: this should take the Ecology as a parameter, instead of accessing it statically.
 */
private [spaces] class SpaceManagerPersister(val ecology:Ecology) extends Actor with EcologyMember {
  
  lazy val Core = interface[querki.core.Core]
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  lazy val Evolutions = interface[querki.evolutions.Evolutions]
  lazy val SystemInterface = interface[querki.system.System]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]

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
            val id = OID(row[Long]("id"))
            val name = row[String]("name")
            val display = row[String]("display")
            // TODO: this is clearly wrong -- eventually, we will allow handle to be NULL. So we need to also
            // fetch Identity.id, and ThingId that if we don't find a handle:
            val ownerHandle = row[String]("handle")
            SpaceDetails(AsName(name), id, display, AsName(ownerHandle))
          }
        } ++ { if (owner == querki.identity.MOIDs.SystemUserOID) { Seq(SpaceDetails(AsName("System"), SystemIds.systemOID, SystemInterface.State.name, AsName("systemUser"))) } else Seq.empty }
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
                 AND SpaceMembership.membershipState != {ownerState}
              """).on("owner" -> owner.raw, "ownerState" -> MembershipState.owner)()
          spaceStream.force.map { row =>
            val id = OID(row[Long]("id"))
            val name = row[String]("name")
            val display = row[String]("display")
            // TODO: this is clearly wrong -- eventually, we will allow handle to be NULL. So we need to also
            // fetch Identity.id, and ThingId that if we don't find a handle:
            val ownerHandle = row[String]("handle")
            SpaceDetails(AsName(name), id, display, AsName(ownerHandle))
          }
        }
        sender ! MySpaces(mySpaces, memberOf)
    }
    
    // =========================================
    
    case CreateSpacePersist(owner, userMaxSpaces, name, display) => Tryer {
      DB.withTransaction(dbName(System)) { implicit conn =>
        val numWithName = SQL("""
          SELECT COUNT(*) AS c from Spaces 
           WHERE owner = {owner} AND name = {name}
          """).on("owner" -> owner.raw, "name" -> name).apply().headOption.get[Long]("c")
        if (numWithName > 0) {
          throw new PublicException("Space.create.alreadyExists", name)
        }
      
        if (userMaxSpaces < Int.MaxValue) {
          val sql = SQL("""
                SELECT COUNT(*) AS count FROM Spaces
                WHERE owner = {owner}
                """).on("owner" -> owner.id.raw)
          val row = sql().headOption
          val numOwned = row.map(_.long("count")).getOrElse(throw new InternalException("Didn't get any rows back in canCreateSpaces!"))
          if (numOwned >= userMaxSpaces)
            throw new PublicException("Space.create.maxSpaces", userMaxSpaces)
        }
      }
    
      val spaceId = OID.next(ShardKind.User)
    
      // NOTE: we have to do this as several separate Transactions, because part goes into the User DB and
      // part into System. That's unfortunate, but kind of a consequence of the architecture.
      // TODO: disturbingly, we don't seem to be rolling back these transactions if we get, say, an exception
      // thrown during this code! WTF?!? Dig into this more carefully: we have deeper problems if we can't count
      // upon reliable rollback. Yes, each of these is a separate transaction, but I've seen instances where one
      // of the updates in the first transaction block failed, and the table was nonetheless created.
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        SpacePersistence.SpaceSQL(spaceId, """
            CREATE TABLE {tname} (
              id bigint NOT NULL,
              model bigint NOT NULL,
              kind int NOT NULL,
              props MEDIUMTEXT NOT NULL,
              PRIMARY KEY (id))
            """).executeUpdate()
      }
      DB.withTransaction(dbName(System)) { implicit conn =>
        SQL("""
            INSERT INTO Spaces
            (id, shard, name, display, owner, size) VALUES
            ({sid}, {shard}, {name}, {display}, {ownerId}, 0)
            """).on("sid" -> spaceId.raw, "shard" -> 1.toString, "name" -> name,
                    "display" -> display, "ownerId" -> owner.raw).executeUpdate()
      }
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        // We need to evolve the Space before we try to create anything in it:
        Evolutions.checkEvolution(spaceId, 1)
        val initProps = Core.toProps(Core.setName(name), DisplayNameProp(display))()
        SpacePersistence.createThingInSql(spaceId, spaceId, SystemIds.systemOID, Kind.Space, initProps, DateTime.now, SystemInterface.State)        
      }
      
      Changed(spaceId, DateTime.now)      
    } 
    { sender ! _ }
    { sender ! ThingError(_) }
    
    case GetSpaceByName(ownerId:OID, name:String) => {
      val result = DB.withTransaction(dbName(System)) { implicit conn =>
        val rowOption = SQL("""
            SELECT id from Spaces WHERE owner = {ownerId} AND name = {name}
            """).on("ownerId" -> ownerId.raw, "name" -> name)().headOption
        rowOption.map(row => OID(row[Long]("id")))
      }
      result match {
        case Some(id) => sender ! SpaceId(id)
        case None => sender ! ThingError(new PublicException("Thing.find.noSuch"))
      }
    }
  }
}