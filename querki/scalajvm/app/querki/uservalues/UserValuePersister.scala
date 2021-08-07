package querki.uservalues

import akka.actor._
import akka.event.LoggingReceive

import anorm.{Success => AnormSuccess, _}
import anorm.SqlParser._
import play.api.db._

import org.querki.requester._

import models.OID

import querki.db._
import querki.db.ShardKind._
import querki.ecology._
import querki.identity.{Identity, PublicIdentity}
import querki.identity.IdentityCacheMessages._
import querki.spaces.messages.SpaceSubsystemRequest
import querki.time.DateTime
import querki.time.TimeAnorm._
import querki.util.QLog
import querki.util.SqlHelpers._
import querki.values.{EmptyValue, QValue, SpaceState}

import PersistMessages._

private[uservalues] class UserValuePersister(
  val spaceId: OID,
  implicit
  val ecology: Ecology
) extends Actor
     with EcologyMember
     with Requester {

  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val UserValues = interface[querki.uservalues.UserValues]

  def SpaceSQL(query: String): SqlQuery = SpacePersistence.SpaceSQL(spaceId, query)

  private case class RawUserValue(
    identId: OID,
    propId: OID,
    thingId: OID,
    valStr: String,
    modTime: DateTime
  ) {

    def v(implicit state: SpaceState): QValue = {
      state.prop(propId) match {
        case Some(prop) => prop.deserialize(valStr)
        case None       => { QLog.error("LoadValuesForUser got unknown Property " + propId); EmptyValue.untyped }
      }
    }

    def toOneUserValue(identity: PublicIdentity)(implicit state: SpaceState): OneUserValue = {
      OneUserValue(identity, thingId, propId, v, modTime)
    }
  }

  private val rawUVParser =
    for {
      identId <- oid("identityId")
      propId <- oid("propertyId")
      thingId <- oid("thingId")
      valStr <- str("propValue")
      modTime <- dateTime("modTime")
    } yield RawUserValue(identId, propId, thingId, valStr, modTime)

  def receive = LoggingReceive {
    case LoadValuesForUser(identity, state) => {
      QDB(ShardKind.User) { implicit conn =>
        val rawUVs = SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE identityId = {identityId}
	          """)
          .on("identityId" -> identity.id.raw)
          .as(rawUVParser.*)

        implicit val s = state
        val uvs = rawUVs.map(_.toOneUserValue(identity))

        sender ! ValuesForUser(uvs)
      }
    }

    case SpaceSubsystemRequest(req, space, LoadThingPropValues(thingId, propId, state)) => {
      val rawUVs = QDB(ShardKind.User) { implicit conn =>
        SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE propertyId = {propertyId}
                 AND thingId = {thingId}
	          """)
          .on("propertyId" -> propId.raw, "thingId" -> thingId.raw)
          .as(rawUVParser.*)
      }

      // This must happen *after* we close the Transaction, because we're about to do an asynchronous roundtrip to
      // the IdentityCache:
      val idents = rawUVs.map(_.identId)
      loopback(IdentityAccess.getIdentities(idents)).foreach { identities =>
        implicit val s = state
        val results =
          rawUVs.map(raw => raw.toOneUserValue(identities.get(raw.identId).getOrElse(Identity.AnonymousIdentity)))
        sender ! ValuesForUser(results)
      }
    }

    case SpaceSubsystemRequest(req, space, LoadUserPropValues(identity, state)) => {
      QDB(ShardKind.User) { implicit conn =>
        val rawUVs = SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE identityId = {identityId}
	          """)
          .on("identityId" -> identity.id.raw)
          .as(rawUVParser.*)

        implicit val s = state
        val uvs = rawUVs.map(_.toOneUserValue(identity))
        sender ! ValuesForUser(uvs)
      }
    }

    // TODO: this should probably get refactored with LoadThingPropValues above -- they are close
    // to identical:
    case SpaceSubsystemRequest(req, space, LoadAllPropValues(propId, state)) => {
      val rawUVs = QDB(ShardKind.User) { implicit conn =>
        SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE propertyId = {propertyId}
	          """)
          .on("propertyId" -> propId.raw)
          .as(rawUVParser.*)
      }

      // This must happen *after* we close the Transaction, because we're about to do an asynchronous roundtrip to
      // the IdentityCache:
      val idents = rawUVs.map(_.identId)
      loopback(IdentityAccess.getIdentities(idents)).foreach { identities =>
        implicit val s = state
        val results =
          rawUVs.map(raw => raw.toOneUserValue(identities.get(raw.identId).getOrElse(Identity.AnonymousIdentity)))
        sender ! ValuesForUser(results)
      }
    }

    case SaveUserValue(uv, state, update) => {
      QDB(ShardKind.User) { implicit conn =>
        implicit val s = state
        val prop = state.prop(uv.propId).getOrElse(
          throw new Exception("SaveUserValue is trying to serialize unknown Property " + uv.propId)
        )
//        val uvType = UserValues.getUserType(prop.pType).getOrElse(throw new Exception("SaveUserValue is trying to serialize non-UV Property " + uv.propId))

        if (uv.v.isDeleted) {
          SpaceSQL("""
            DELETE FROM {uvname}
             WHERE thingId = {thingId} AND propertyId = {propertyId} AND identityId = {identityId}
            """).on(
            "thingId" -> uv.thingId.raw,
            "propertyId" -> uv.propId.raw,
            "identityId" -> uv.identity.id.raw
          ).executeUpdate
        }
        // Have I ever mentioned how much I despise SQL's lack of a standard UPSERT operator?
        // TODO: replace these clauses with a single MySQL INSERT ... ON DUPLICATE KEY ...
        else if (update) {
          SpaceSQL("""
            UPDATE {uvname}
               SET propValue = {propValue}, modTime = {modTime}
             WHERE thingId = {thingId} AND propertyId = {propertyId} AND identityId = {identityId}
            """).on(
            "thingId" -> uv.thingId.raw,
            "propertyId" -> uv.propId.raw,
            "identityId" -> uv.identity.id.raw,
            "propValue" -> prop.serialize(uv.v),
            "modTime" -> uv.modTime
          ).executeUpdate
        } else {
          // Note that a bunch of the Booleans simply default to false, and are irrelevant for a new Comment:
          SpaceSQL("""
            INSERT INTO {uvname}
            (  thingId,   propertyId,   identityId,   propValue,   modTime) VALUES
            ( {thingId}, {propertyId}, {identityId}, {propValue}, {modTime})
            """).on(
            "thingId" -> uv.thingId.raw,
            "propertyId" -> uv.propId.raw,
            "identityId" -> uv.identity.id.raw,
            "propValue" -> prop.serialize(uv.v),
            "modTime" -> uv.modTime
          ).executeUpdate
        }
      }

    }
  }
}
