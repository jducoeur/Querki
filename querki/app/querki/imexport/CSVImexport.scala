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
    
    val rows:Seq[String] = cols.flatMap(_.getTitles).map(escapeCSVCell(_)).mkString(",") +: instances.map { instance =>
      val cellValues:Seq[QValue] = cols.flatMap(_.getPropValue(Some(instance)))
      val cellStrs:Seq[String] = cellValues.map { qv =>
        if (qv.isEmpty)
          ""
        else
          // TODO: for the time being, we use exactly the first element of a List or Set,
          // in order to keep things "square". This isn't necessarily an optimal solution;
          // we may need to add config params to let the Model or Property specify how to
          // do this:
          escapeCSVCell(qv.pType.toUser(qv.first))
      }
      cellStrs.mkString(",")
    }
    // According to Wikipedia (which I will take to be clueful), there is no solid standard, but DOS-style CRLF is
    // usual for CSV:
    val text = rows.mkString("\r\n")
    
    ExportedContentImpl(text.getBytes(), model.displayName + ".csv")
  }
  
  // Again, escaping as suggested by Wikipedia's summary of RFC 4180:
  private def escapeCSVCell(str:String):String = {
    if (str.contains("\n") || str.contains("\"") || str.contains(",")) {
      s""""${str.replace("\"", "\"\"")}""""
    } else
      str
  }
}
