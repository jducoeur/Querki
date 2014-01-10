package querki

import anorm.SqlQuery

import models.{OID, PType}
import models.Thing.PropMap

import querki.ecology._

import querki.values.SpaceState

package object spaces {
  // This is a pure marker trait, indicating that this PropValue didn't load correctly yet:
  trait UnresolvedPropValue

  // The name of the Space Actor
  def sid(id:OID):String = id.toString
    
  trait SpacePersistence extends EcologyInterface {
    def UnresolvedPropType:PType[String]
    
    // The name of the Space's Thing Table
    def thingTable(id:OID)
    
    def SpaceSQL(spaceId:OID, query:String, version:Int = 0):SqlQuery
    def AttachSQL(spaceId:OID, query:String):SqlQuery

    def serializeProps(props:PropMap, space:SpaceState):String
    def deserializeProps(str:String, space:SpaceState):PropMap
    def createThingInSql(thingId:OID, spaceId:OID, modelId:OID, kind:Int, props:PropMap, serialContext:SpaceState)(implicit conn:java.sql.Connection):Int
  }
}