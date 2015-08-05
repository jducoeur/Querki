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
      val rawState = importType match {
        case ImportXML => {
          QLog.spew("I've received the entire XML:")
          val xml = new String(chunkBuffer.toArray)
          QLog.spew(xml)
          new RawXMLImport(rc)(ecology).readXML(xml)
        }
        
        case ImportMySQL => {
          QLog.error("ImportSpaceActor(ImportMySQL) is NYI!")
          throw new Exception("ImportMySQL NYI!")
        }
        
        case _ => {
          QLog.error(s"ImportSpaceActor called with unknown ImportDataType $importType")
          throw new Exception("Unknown ImportDataType!")
        }
      }
      
      QLog.spew(s"Built the SpaceState: $rawState")
      sender ! "Done"
      context.stop(self)
    }
  }
}

object ImportSpaceActor {
  def actorProps(ecology:Ecology, importType:ImportDataType) = Props(classOf[ImportSpaceActor], ecology, importType)
}

sealed trait ImportDataType
case object ImportXML extends ImportDataType
case object ImportMySQL extends ImportDataType
