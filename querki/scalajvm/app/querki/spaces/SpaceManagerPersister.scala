package querki.spaces

import scala.util._

import akka.actor._

import anorm.{Success=>AnormSuccess,_}
import anorm.SqlParser._
import play.api.db._

import com.github.nscala_time.time.Imports._

import org.querki.requester._

import messages._

import models.{AsName, OID}
import models.{Kind, Thing}

import querki.db._
import ShardKind._

import querki.cluster.OIDAllocator._
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
private [spaces] class SpaceManagerPersister(e:Ecology) extends Actor with Requester with EcologyMember {
  
  implicit val ecology = e
  
  lazy val useNewPersist = Config.getBoolean("querki.space.newPersist", false)
  
  lazy val Core = interface[querki.core.Core]
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  lazy val Evolutions = interface[querki.evolutions.Evolutions]
  lazy val QuerkiCluster = interface[querki.cluster.QuerkiCluster]
  lazy val SystemInterface = interface[querki.system.System]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  
  // TODO: this is clearly wrong -- eventually, we will allow handle to be NULL. So we need to also
  // fetch Identity.id, and ThingId that if we don't find a handle:
  private val parseSpaceDetails =
    oid("id") ~ str("name") ~ str("display") ~ str("handle") map 
      { case id ~ name ~ display ~ ownerHandle => SpaceDetails(AsName(name), id, display, AsName(ownerHandle)) }

  def receive = {
    case ListMySpaces(owner) => {
        // TODO: this involves DB access, so should be async using the Actor DSL
        // Note that Spaces are now indexed by Identity, so we need to indirect
        // through that table. (Since our parameter is User OID.)
        val mySpaces = QDB(System) { implicit conn =>
          SQL("""
              SELECT Spaces.id, Spaces.name, display, Identity.handle FROM Spaces
                JOIN Identity ON userid={owner}
              WHERE owner = Identity.id
                AND status = 0
              """)
            .on("owner" -> owner.raw)
            .as(parseSpaceDetails.*)
        } ++ { if (owner == querki.identity.MOIDs.SystemUserOID) { Seq(SpaceDetails(AsName("System"), SystemIds.systemOID, SystemInterface.State.name, AsName("systemUser"))) } else Seq.empty }
        val memberOf = QDB(System) { implicit conn =>
          // Note that this gets a bit convoluted, by necessity. We are coming in through a User;
          // translating that to Identities; getting all of the Spaces that those Identities are members of;
          // then getting the Identities that own those Spaces so we can get their handles. Hence the
          // need to alias the Identity table.
          SQL("""
              SELECT Spaces.id, Spaces.name, display, OwnerIdentity.handle FROM Spaces
                JOIN Identity AS RequesterIdentity ON userid={owner}
                JOIN SpaceMembership ON identityId=RequesterIdentity.id
                JOIN Identity AS OwnerIdentity ON OwnerIdentity.id=Spaces.owner
               WHERE Spaces.id = SpaceMembership.spaceId
                 AND SpaceMembership.membershipState != {ownerState}
              """)
            .on("owner" -> owner.raw, "ownerState" -> MembershipState.owner)
            .as(parseSpaceDetails.*)
        }
        sender ! MySpaces(mySpaces, memberOf)
    }
    
    // =========================================
    
    case CreateSpacePersist(owner, userMaxSpaces, name, display, initialStatus) => {
      try {
        QDB(System) { implicit conn =>
          val numWithName = SQL("""
              SELECT COUNT(*) AS c from Spaces 
               WHERE owner = {owner} AND name = {name} AND status = 0
              """)
            .on("owner" -> owner.raw, "name" -> name)
            .as(long("c").single)
          if (numWithName > 0) {
            throw new PublicException("Space.create.alreadyExists", name)
          }
        
          if (userMaxSpaces < Int.MaxValue) {
            val numOwned = SQL("""
                  SELECT COUNT(*) AS count FROM Spaces
                  WHERE owner = {owner} AND status = 0
                  """)
                .on("owner" -> owner.id.raw)
                .as(long("count").single)
            if (numOwned >= userMaxSpaces)
              throw new PublicException("Space.create.maxSpaces", userMaxSpaces)
          }
        }
        
        // Going back to the old way of doing things until we have a more reliable Akka Persistence
        // implementation:
//        QuerkiCluster.oidAllocator.request(NextOID) map { case NewOID(spaceId) =>
        {
          val spaceId = OID.next(ShardKind.User)
          // NOTE: we have to do this as several separate Transactions, because part goes into the User DB and
          // part into System. That's unfortunate, but kind of a consequence of the architecture.
          // TODO: disturbingly, we don't seem to be rolling back these transactions if we get, say, an exception
          // thrown during this code! WTF?!? Dig into this more carefully: we have deeper problems if we can't count
          // upon reliable rollback. Yes, each of these is a separate transaction, but I've seen instances where one
          // of the updates in the first transaction block failed, and the table was nonetheless created.
          // TODO: once Conversations and User Values have been moved to Akka Persistence, we should no longer need
          // to create and evolve the Space Table here. At that point, only the insertion into the Spaces table should
          // be needed any more.
          QDB(User) { implicit conn =>
            SpacePersistence.SpaceSQL(spaceId, """
                CREATE TABLE {tname} (
                  id bigint NOT NULL,
                  model bigint NOT NULL,
                  kind int NOT NULL,
                  props MEDIUMTEXT NOT NULL,
                  PRIMARY KEY (id))
                  DEFAULT CHARSET=utf8
                """).executeUpdate()
          }
          QDB(System) { implicit conn =>
            SQL("""
                INSERT INTO Spaces
                (id, shard, name, display, owner, size, status) VALUES
                ({sid}, {shard}, {name}, {display}, {ownerId}, 0, {status})
                """).on("sid" -> spaceId.raw, "shard" -> 1.toString, "name" -> name,
                        "display" -> display, "ownerId" -> owner.raw, "status" -> initialStatus.underlying).executeUpdate()
          }
          QDB(User) { implicit conn =>
            // We need to evolve the Space before we try to create anything in it:
            Evolutions.checkEvolution(spaceId, 1)
            if (!useNewPersist) {
              val initProps = Core.toProps(Core.setName(name), DisplayNameProp(display))
              SpacePersistence.createThingInSql(spaceId, spaceId, SystemIds.systemOID, Kind.Space, initProps, DateTime.now, SystemInterface.State)        
            }
          }
          
          sender ! Changed(spaceId, DateTime.now)
        } 
      } catch {
        case ex:PublicException => sender ! ThingError(ex)
      }
    }
    
    case GetSpaceByName(ownerId:OID, name:String) => {
      val result = QDB(System) { implicit conn =>
        SQL("""
            SELECT id from Spaces 
             WHERE owner = {ownerId} 
               AND name = {name}
               AND status = 0
            """)
          .on("ownerId" -> ownerId.raw, "name" -> name)
          .as(oid("id").singleOpt)
      }
      result match {
        case Some(id) => sender ! SpaceId(id)
        case None => sender ! ThingError(new PublicException("Thing.find.noSuch"))
      }
    }
    
    case GetSpaceCount(requester) => {
      val result = QDB(System) { implicit conn =>
        SQL("SELECT COUNT(*) AS c FROM Spaces WHERE status = 0")
          .as(long("c").single)
      }
      sender ! SpaceCount(result)
    }
    
    case ChangeSpaceStatus(spaceId, newStatus) => {
      QDB(System) { implicit conn =>
        SQL("""
          UPDATE Spaces
             SET status = {status}
           WHERE id = {spaceId}
        """).on("spaceId" -> spaceId.raw, "status" -> newStatus.underlying).executeUpdate()
      }
      sender ! StatusChanged
    }
  }
}