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
  
  lazy val csv = new CSVImexport
  
  def exportInstances(rc:RequestContext, format:Format, model:Thing)(implicit state:SpaceState):ExportedContent = {
    if (!state.canRead(rc.requesterOrAnon, model.id))
      throw new PublicException("Imexport.exportNotAllowed", model.displayName)
    
    val instances = state.descendants(model.id, false, true).
    					filter(thing => state.canRead(rc.requesterOrAnon, thing.id)).
    					toSeq.
    					sortBy(_.displayName)
    					
    val exporter:Exporter = format match {
      case Format.CSV => csv
      case _ => throw new Exception("Unknown Exporter specified: " + format)
    }
    
    exporter.exportInstances(model, instances)
  }
}