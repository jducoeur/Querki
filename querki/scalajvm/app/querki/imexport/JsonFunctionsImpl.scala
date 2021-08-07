package querki.imexport

import querki.api.{AutowireParams, SpaceApiImpl}
import querki.data.TID
import querki.globals._
import querki.values.QLRequestContext

class JsonFunctionsImpl(info: AutowireParams)(implicit e: Ecology) extends SpaceApiImpl(info, e) with JsonFunctions {
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val QL = interface[querki.ql.QL]

  def doRoute(req: Request): Future[String] = route[JsonFunctions](this)(req)

  def getJsonFor(
    thingId: TID,
    propIdOpt: Option[TID]
  ): Future[String] = withThing(thingId) { thing =>
    implicit val s = state
    val exporter = new JsonExport
    propIdOpt match {
      case Some(propId) => withProp(propId) { prop =>
          // They specified a particular Property:
          val qv = thing.getPropVal(prop)
          if (qv.pType == Basic.QLType) {
            // Special case: if it is a Function, we *process* that, and JSONify the result:
            val processedOpt = for {
              ql <- qv.firstTyped(Basic.QLType)
            } yield QL.processMethod(ql, QLRequestContext(rc), None, Some(thing), None)

            processedOpt.map(_.map(processed => exporter.jsonify(processed, false))).getOrElse(fut("null"))
          } else
            fut(exporter.jsonify(qv, false))
        }
      case None => {
        // No Property, so render the Thing itself by sending in the OID:
        fut(exporter.jsonify(Core.ExactlyOne(Core.LinkType(thing.id))))
      }
    }
  }
}
