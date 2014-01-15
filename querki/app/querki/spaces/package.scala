package querki

import anorm.SqlQuery

import models.{OID, PType, Thing}
import models.Thing.PropMap

import querki.ecology._
import querki.util.Sequencer
import querki.values.SpaceState

package object spaces {
  // This is a pure marker trait, indicating that this PropValue didn't load correctly yet:
  trait UnresolvedPropValue

  // The name of the Space Actor
  def sid(id:OID):String = id.toString
    
  trait SpacePersistence extends EcologyInterface {
    def UnresolvedPropType:PType[String]
    
    // The name of the Space's Thing Table
    def thingTable(id:OID):String
    
    def SpaceSQL(spaceId:OID, query:String, version:Int = 0):SqlQuery
    def AttachSQL(spaceId:OID, query:String):SqlQuery

    def serializeProps(props:PropMap, space:SpaceState):String
    def deserializeProps(str:String, space:SpaceState):PropMap
    def createThingInSql(thingId:OID, spaceId:OID, modelId:OID, kind:Int, props:PropMap, serialContext:SpaceState)(implicit conn:java.sql.Connection):Int
  }
    
  case class ThingChangeRequest(state:SpaceState, modelIdOpt:Option[OID], thingOpt:Option[Thing], newProps:PropMap)

  trait SpaceChangeManager extends EcologyInterface {
    /**
     * Called before every Create or Modify operation. Listeners can use this specifically to edit the Props.
     * 
     * IMPORTANT: this is called by the Space, so it MUST NOT BLOCK.
     */
    def thingChanges:Sequencer[ThingChangeRequest]
  }
}