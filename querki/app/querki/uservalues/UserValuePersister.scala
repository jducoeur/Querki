package querki.uservalues

import akka.actor._
import akka.event.LoggingReceive

import anorm.{Success=>AnormSuccess,_}
import play.api.db._
import play.api.Play.current

import models.OID

import querki.db._
import querki.db.ShardKind._
import querki.ecology._
import querki.identity.Identity
import querki.identity.IdentityCacheMessages._
import querki.spaces.messages.UserValuePersistRequest
import querki.time.DateTime
import querki.time.TimeAnorm._
import querki.util.{QLog, Requester}
import querki.util.SqlHelpers._
import querki.values.{EmptyValue, QValue}

import PersistMessages._

private[uservalues] class UserValuePersister(val spaceId:OID, implicit val ecology:Ecology) extends Actor with EcologyMember with Requester {
  
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val UserValues = interface[querki.uservalues.UserValues]
  
  lazy val identityCache = IdentityAccess.identityCache
  
  def SpaceSQL(query:String):SqlQuery = SpacePersistence.SpaceSQL(spaceId, query)
  
  def receive = LoggingReceive {
    case LoadValuesForUser(identity, state) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        val valueStream = SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE identityId = {identityId}
	          """).on("identityId" -> identity.id.raw)()
	          
	    implicit val s = state
	    val uvs = valueStream.map { row =>
	      val identId = row.oid("identityId")
          val propId = row.oid("propertyId")
          val thingId = row.oid("thingId")
          val valStr = row.string("propValue")
          val modTime = row.dateTime("modTime")
          val v = state.prop(propId) match {
            case Some(prop) => prop.deserialize(valStr)
            case None => { QLog.error("LoadValuesForUser got unknown Property " + propId); EmptyValue.untyped }
          }
          OneUserValue(identity, thingId, propId, v, modTime)
        }
        
        sender ! ValuesForUser(uvs.force)
      }
    }
    
    case UserValuePersistRequest(req, own, space, LoadThingPropValues(thingId, propId, state)) => {
      val tuples = DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        val valueStream = SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE propertyId = {propertyId}
                 AND thingId = {thingId}
	          """).on("propertyId" -> propId.raw, "thingId" -> thingId.raw)()
	          
	    implicit val s = state
	    val uvs = valueStream.map { row =>
	      val identId = row.oid("identityId")
          val propId = row.oid("propertyId")
          val thingId = row.oid("thingId")
          val valStr = row.string("propValue")
          val modTime = row.dateTime("modTime")
          val v = state.prop(propId) match {
            case Some(prop) => prop.deserialize(valStr)
            case None => { QLog.error("LoadValuesForUser got unknown Property " + propId); EmptyValue.untyped }
          }
          (identId, thingId, propId, v, modTime)
        }
        uvs.force
      }
      
      // This must happen *after* we close the Transaction, because we're about to do an asynchronous roundtrip to
      // the IdentityCache:
      val idents = tuples.map(_._1)
      identityCache.request(GetIdentities(idents)) {
        case IdentitiesFound(identities) => {
          val results = tuples.map(tuple => 
            OneUserValue(
              identities.get(tuple._1).getOrElse(Identity.AnonymousIdentity),
              tuple._2,
              tuple._3,
              tuple._4,
              tuple._5))
          sender ! ValuesForUser(results)
        }
      }
    }
    
    case UserValuePersistRequest(req, own, space, LoadUserPropValues(identity, state)) => {
      val tuples = DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        val valueStream = SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE identityId = {identityId}
	          """).on("identityId" -> identity.id.raw)()
	          
	    implicit val s = state
	    val uvs = valueStream.map { row =>
	      val identId = row.oid("identityId")
          val propId = row.oid("propertyId")
          val thingId = row.oid("thingId")
          val valStr = row.string("propValue")
          val modTime = row.dateTime("modTime")
          val v = state.prop(propId) match {
            case Some(prop) => prop.deserialize(valStr)
            case None => { QLog.error("LoadValuesForUser got unknown Property " + propId); EmptyValue.untyped }
          }
          OneUserValue(identity, thingId, propId, v, modTime)
        }
        sender ! ValuesForUser(uvs.force)
      }
    }
    
    case SaveUserValue(uv, state, update) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        implicit val s = state
        val prop = state.prop(uv.propId).getOrElse(throw new Exception("SaveUserValue is trying to serialize unknown Property " + uv.propId))
//        val uvType = UserValues.getUserType(prop.pType).getOrElse(throw new Exception("SaveUserValue is trying to serialize non-UV Property " + uv.propId))
        
        // Have I ever mentioned how much I despise SQL's lack of a standard UPSERT operator?
        // TODO: replace these clauses with a single MySQL INSERT ... ON DUPLICATE KEY ...
        if (update) {
          SpaceSQL("""
            UPDATE {uvname}
               SET propValue = {propValue}, modTime = {modTime}
             WHERE thingId = {thingId} AND propertyId = {propertyId} AND identityId = {identityId}
            """).on(
                "thingId" -> uv.thingId.raw,
                "propertyId" -> uv.propId.raw,
                "identityId" -> uv.identity.id.raw,
                "propValue" -> prop.serialize(uv.v),
                "modTime" -> uv.modTime).executeUpdate
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
                "modTime" -> uv.modTime).executeUpdate
        }
      }
      
    }
  }
}