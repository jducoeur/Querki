package querki.imexport

import models.{MIMEType, Thing}

import querki.ecology._
import querki.util._
import querki.values.{RequestContext, SpaceState}

object MOIDs extends EcotIds(33) {
  val JsonifyFunctionOID = moid(1)
}

trait Exporter {
  def exportInstances(model:Thing, instances:Seq[Thing])(implicit state:SpaceState):ExportedContent
}

/**
 * For now, this is just a trivial implementation of the interface. We're separating them mostly on
 * principle.
 */
case class ExportedContentImpl(content:Array[Byte], name:String, mime:MIMEType.MIMEType) extends ExportedContent

class ImexportEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with Imexport {
  
  import MOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val Session = interface[querki.session.Session]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    // ImexportFunctions runs in the context of the UserSpaceSession:
    ApiRegistry.registerApiImplFor[ImexportFunctions, ImexportFunctionsImpl](SpaceOps.spaceRegion)
    // ImportSpaceFunctions runs in the context of the UserSession:
    ApiRegistry.registerApiImplFor[ImportSpaceFunctions, ImportSpaceFunctionsImpl](Session.sessionManager)
  }
  
  lazy val csv = new CSVImexport
  
  def exportInstances(rc:RequestContext, format:Format, model:Thing)(implicit state:SpaceState):ExportedContent = {
    if (!AccessControl.canRead(state, rc.requesterOrAnon, model.id))
      throw new PublicException("Imexport.exportNotAllowed", model.displayName)
    
    val instances = state.descendants(model.id, false, true).
              filter(thing => AccessControl.canRead(state, rc.requesterOrAnon, thing.id)).
              toSeq.
              sortBy(_.displayName)
              
    val exporter:Exporter = format match {
      case Format.CSV => csv
      case _ => throw new Exception("Unknown Exporter specified: " + format)
    }
    
    exporter.exportInstances(model, instances)
  }
  
  def exportSpace(rc:RequestContext)(implicit state:SpaceState):String = {
    // For now, only the Owner is allowed to Export
    // TODO: this should really be a permission, that Manager and Owner have
    if (!rc.requesterOrAnon.hasIdentity(state.owner))
      throw new PublicException("Imexport.ownerOnly")
    
    (new XMLExporter).exportSpace(state)
  }

  /***********************************************
   * FUNCTIONS
   ***********************************************/

  lazy val JsonifyFunction = new InternalMethod(JsonifyFunctionOID,
    toProps(
      setName("_JSONify"),
      Categories(ImportExportCategory),
      SkillLevel(SkillLevelAdvanced),
      Summary("Turn the received value into JSON"),
      Signature(
        expected = Some(Seq.empty, "Anything"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (Basic.PlainTextType, "The JSON representation of the received values.")
      ),
      Details("""Sometimes you want to export some data from Querki. One of the common export formats is JSON
                |(JavaScript Object Notation). This function takes any Querki value, and renders it as JSON.
                |
                |As currently written, this will *always* produce a top-level array of values. This is highly
                |subject to change.
                |
                |It can be convenient to combine this with [[_makePropertyBundle._self]], using that to define
                |exactly the data structure you want, and this to turn that into JSON.
                |
                |**Important:** as of this writing, this function is pretty experimental. It doesn't yet deal
                |with any arbitrary Value (in particular, it intentionally does *not* deal with Thing Properties
                |yet), and it is subject to change. If you have opinions about it, please write to us. We are
                |likely to add real JSON APIs down the road, and that might supercede this function.""".stripMargin)))
  {
    override def qlApply(inv: Invocation): QFut = {
      implicit val state = inv.state
      val exporter = new JsonExport()
      for {
        qv <- inv.contextValue
        jsv = exporter.jsonify(qv)
      }
        yield ExactlyOne(Basic.PlainTextType(jsv))
    }
  }
  
  override lazy val props = Seq(
    JsonifyFunction
  )
}
