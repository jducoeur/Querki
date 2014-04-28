package querki.uservalues

import akka.actor._

import anorm.{Success=>AnormSuccess,_}
import play.api.db._
import play.api.Play.current

import models.OID

import querki.db._
import querki.db.ShardKind._
import querki.ecology._
import querki.time.DateTime
import querki.time.TimeAnorm._
import querki.util.QLog
import querki.util.SqlHelpers._
import querki.values.{EmptyValue, QValue}

import PersistMessages._

private[uservalues] class UserValuePersister(val spaceId:OID, implicit val ecology:Ecology) extends Actor with EcologyMember {
  
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val UserValues = interface[querki.uservalues.UserValues]
  
  def SpaceSQL(query:String):SqlQuery = SpacePersistence.SpaceSQL(spaceId, query)
  
  def receive = {
    case LoadValuesForUser(identityId, state) => {
      DB.withTransaction(dbName(ShardKind.User)) { implicit conn =>
        val valueStream = SpaceSQL("""
	          SELECT * FROM {uvname} 
               WHERE identityId = {identityId}
	          """).on("identityId" -> identityId.raw)()
	          
	    implicit val s = state
	    val uvs = valueStream.map { row =>
          val propId = row.oid("propertyId")
          val thingId = row.oid("thingId")
          val valStr = row.string("propValue")
          val modTime = row.dateTime("modTime")
          val v = state.prop(propId) match {
            case Some(prop) => { prop.deserialize(valStr)
//              UserValues.getUserType(prop.pType) match {
//                // Ideally, this would be prop.deserialize(), but we need to use the userType:
//                case Some(uvType) => prop.cType.deserialize(valStr, uvType)
//                case None => { QLog.error("LoadValuesForUser.deserialize found non-UV Property " + propId); EmptyValue.untyped }
//              }
            }
            case None => { QLog.error("LoadValuesForUser got unknown Property " + propId); EmptyValue.untyped }
          }
          OneUserValue(thingId, propId, v, modTime)
        }
        
        sender ! ValuesForUser(identityId, uvs.force)
      }
    }
    
    case SaveUserValue(identityId, uv, state, update) => {
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
                "identityId" -> identityId.raw,
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
                "identityId" -> identityId.raw,
                "propValue" -> prop.serialize(uv.v),
                "modTime" -> uv.modTime).executeUpdate
        }
      }
      
    }
  }
}