package querki.imexport

import akka.actor._

import org.querki.requester._

import querki.globals._
import querki.streaming._
import UploadMessages._

/**
 * This does the actual heavy lifting of importing a data file from the client,
 * and building a Space from it.
 * 
 * This Actor is created from ImportSpaceFunctions, and is attached to a UserSession.
 * 
 * @author jducoeur
 */
class ImportSpaceActor(ecology:Ecology, importType:ImportDataType) extends Actor with Requester with UploadActor {
  def receive = handleChunks orElse {
    case UploadComplete(rc) => {
      val xml = new String(chunkBuffer.toArray)
      val rawState = new RawXMLImport(rc)(ecology).readXML(xml)
    }
  }
}

object ImportSpaceActor {
  def actorProps(ecology:Ecology, importType:ImportDataType) = Props(classOf[ImportSpaceActor], ecology, importType)
}

sealed trait ImportDataType
case object ImportXML extends ImportDataType
case object ImportMySQL extends ImportDataType
