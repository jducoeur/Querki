package querki.imexport

import akka.actor._

import upickle._

import org.querki.requester._

import querki.globals._
import querki.streaming._
import UploadMessages._
import querki.values.RequestContext

/**
 * This does the actual heavy lifting of importing a data file from the client,
 * and building a Space from it.
 * 
 * This Actor is created from ImportSpaceFunctions, and is attached to a UserSession.
 * 
 * @author jducoeur
 */
class ImportSpaceActor(val ecology:Ecology, importType:ImportDataType, name:String) extends Actor with Requester 
  with UploadActor with ImporterImpl with EcologyMember 
{
  lazy val ClientApi = interface[querki.api.ClientApi]
  
  def buildSpaceState(rc:RequestContext):SpaceState = {
    importType match {
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
  }
  
  def processBuffer(rc:RequestContext) = {
    if (rc.requester.isEmpty)
      throw new Exception("Somehow got to ImportSpaceActor when not logged in!")
    
    val rawState = buildSpaceState(rc)
    QLog.spew(s"Built the SpaceState: $rawState")
    
    loopback(createSpaceFromImported(rc.requesterOrAnon, name)(rawState)) foreach { spaceInfo =>
      QLog.spew(s"Successfully constructed Space ${spaceInfo.display}")
      val apiInfo = ClientApi.spaceInfo(spaceInfo)
      sender ! UploadProcessSuccessful(write(apiInfo))
      context.stop(self)
    }
  }
}

object ImportSpaceActor {
  def actorProps(ecology:Ecology, importType:ImportDataType, name:String) = Props(classOf[ImportSpaceActor], ecology, importType, name)
}

sealed trait ImportDataType
case object ImportXML extends ImportDataType
case object ImportMySQL extends ImportDataType
