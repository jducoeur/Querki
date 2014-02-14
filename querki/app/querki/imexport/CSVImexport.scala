package querki.imexport

import models.Thing

import querki.ecology._
import querki.values.{QValue, SpaceState}

/**
 * Deals with import and export of CSV files.
 */
private[imexport] class CSVImexport(implicit val ecology:Ecology) extends Exporter with SquareExporter with EcologyMember {
  def exportInstances(model:Thing, instances:Seq[Thing])(implicit state:SpaceState):ExportedContent = {
    val cols = columns(model)
    
    val rows:Seq[String] = instances.map { instance =>
      val cellValues:Seq[QValue] = cols.flatMap(_.getPropValue(Some(instance)))
      val cellStrs:Seq[String] = cellValues.map { qv =>
        if (qv.isEmpty)
          ""
        else
          qv.pType.toUser(qv.first)
      }
      // TODO: do all the escaping needed for these strings:
      cellStrs.mkString(",")
    }
    val text = rows.mkString("\r\n")
    
    ExportedContentImpl(text.getBytes(), model.displayName + ".csv")
  }
}