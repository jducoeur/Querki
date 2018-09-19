package querki.imexport

import querki.api.{AutowireParams, SpaceApiImpl}
import querki.data.TID
import querki.globals._

class JsonFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with JsonFunctions  {
  lazy val Core = interface[querki.core.Core]
  
  def doRoute(req:Request):Future[String] = route[JsonFunctions](this)(req)
  
  def getJsonFor(thingId: TID, propIdOpt: Option[TID]): Future[String] = withThing(thingId) { thing =>
    implicit val s = state
    val exporter = new JsonExport
    propIdOpt match {
      case Some(propId) => withProp(propId) { prop =>
        val qv = thing.getPropVal(prop)
        fut(exporter.jsonify(qv, false))
      }
      case None => {
        // No Property, so render the Thing itself by sending in the OID:
        fut(exporter.jsonify(Core.ExactlyOne(Core.LinkType(thing.id))))
      }
    }
  }
}
