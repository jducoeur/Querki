package querki.imexport

import models.{MIMEType, Thing}

import querki.ecology._
import querki.util._
import querki.values.{RequestContext, SpaceState}

trait Exporter {
  def exportInstances(model:Thing, instances:Seq[Thing])(implicit state:SpaceState):ExportedContent
}

/**
 * For now, this is just a trivial implementation of the interface. We're separating them mostly on
 * principle.
 */
case class ExportedContentImpl(content:Array[Byte], name:String, mime:MIMEType.MIMEType) extends ExportedContent

class ImexportEcot(e:Ecology) extends QuerkiEcot(e) with Imexport {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val SessionHandlerRegistry = interface[querki.session.SessionHandlerRegistry]
  
  override def postInit() = {
    SessionHandlerRegistry.registerUserSessionImplFor[ImexportFunctions, ImexportFunctionsImpl]
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
}