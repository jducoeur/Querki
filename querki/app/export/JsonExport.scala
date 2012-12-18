package export

import play.api.libs.json._
import Json._

import models._
import models.system._

class SpaceImportExport(state:SpaceState, spaceIdOpt:Option[OID]) {
  
  // Only valid during Import:
  def spaceId = spaceIdOpt.get
  
  implicit object OIDFormat extends Format[OID] {
    def writes(oid:OID):JsValue = JsString(oid.toString)
    def reads(json:JsValue):OID = OID(json.as[String])
  }
  
  import Kind.Kind
  implicit object KindFormat extends Format[Kind] {
    def writes(kind:Kind):JsValue = JsNumber(kind)
    def reads(json:JsValue):Kind = json.as[Long].toInt
  }
  
  import Thing.PropMap
  implicit object PropsFormat extends Format[PropMap] {
    def writes(props:PropMap):JsValue = {
      toJson(props map { entry =>
        val (pid, pVal) = entry
        val prop = state.prop(pid)
        val serialized = prop.serialize(prop.castVal(pVal))
        (pid.toString, serialized)
      })
    }
    def reads(json:JsValue):PropMap = {
      val rawMap = json.as[Map[String, String]]
      rawMap map { rawEntry =>
        val (pidStr, serialized) = rawEntry
        val pid = OID(pidStr)
        // TODO: this is only looking up props against the App, not the Space:
        val prop = state.prop(pid)
        val v = prop.deserialize(serialized)
        (pid -> v)        
      }
    }
  }
  
  // TODO: all the OIDs contained in here need to be remapped, as a second pass!
  implicit object ThingFormat extends Format[ThingState] {
    def writes(thing:ThingState):JsValue = JsObject(List(
      "id" -> toJson(thing.id),
      "model" -> toJson(thing.model.id),
      "kind" -> JsNumber(thing.kind),
      "props" -> toJson(thing.props)
    ))
    def reads(json:JsValue):ThingState = ThingState(
      (json \ "id").as[OID],
      spaceId,
      (json \ "model").as[OID],
      () => (json \ "props").as[PropMap],
      (json \ "kind").as[Kind]
    )
  }
  
  implicit object SpaceFormat extends Format[SpaceState] {
    def writes(state:SpaceState):JsValue = JsObject(List(
      "id" -> toJson(state.id),
      "model" -> toJson(state.model),
      "props" -> toJson(state.props),
      "owner" -> toJson(state.owner),
      "name" -> JsString(state.name),
      "app" -> toJson(state.app map (_.id)),
      "things" -> toJson(state.things map { entry => 
        val (oid, thing) = entry
        (oid.toString, thing)
      })
    ))
    def reads(json:JsValue):SpaceState = SpaceState(
      spaceId,
      (json \ "model").as[OID],  // TODO: needs to be mapped into the new world
      () => (json \ "props").as[PropMap],  // TODO: should be mapped against the new world
      (json \ "owner").as[OID], // TODO: should actually be whoever is importing?
      (json \ "name").as[String], // TODO: check that it's not duplicate
      None, // TODO: App
      Map.empty[OID, PType[_]], // LATER: Types
      Map.empty[OID, Property[_,_,_]], // TODO: Properties
      (json \ "things").as[Map[String, ThingState]] map { entry =>
        (OID(entry._1), entry._2)
      },
      Map.empty[OID, Collection[_]] // LATER: Collections      
    )
  }
  
  def export:JsValue = toJson(state)
  def importSpace(json:JsValue):SpaceState = json.as[SpaceState]
}

object SpaceImportExport {
  def export(state:SpaceState):JsValue = {
    val exporter = new SpaceImportExport(state, None)
    exporter.export
  }
  
  def importSpace(spaceId:OID, json:JsValue):SpaceState = {
    val importer = new SpaceImportExport(SystemSpace.State, Some(spaceId))
    importer.importSpace(json)
  }
}