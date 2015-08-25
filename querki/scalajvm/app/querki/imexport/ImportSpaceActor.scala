package querki.imexport

import scala.util.{Failure, Success}

import akka.actor._

import upickle._

import org.querki.requester._

import querki.globals._
import ImportSpaceFunctions._
import querki.streaming._
import UploadMessages._
import querki.values.RequestContext

import mysql._

/**
 * This does the actual heavy lifting of importing a data file from the client,
 * and building a Space from it.
 * 
 * This Actor is created from ImportSpaceFunctions, and is attached to a UserSession.
 * 
 * @author jducoeur
 */
class ImportSpaceActor(val ecology:Ecology, importType:ImportDataType, name:String, totalSize:Int) extends Actor with Requester 
  with UploadActor with ImporterImpl with ImportProgressInternal with EcologyMember 
{
  import ImportSpaceActor._
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  
  def buildSpaceState(rc:RequestContext):SpaceState = {
    importType match {
      case ImportXML => {
        QLog.spew("I've received the entire XML:")
        val xml = uploaded
        QLog.spew(xml)
        new RawXMLImport(rc)(ecology).readXML(xml)
      }
      
      case ImportMySQL => {
        val sql = uploaded
        QLog.spew(sql)
        new MySQLImport(rc, name)(ecology).readDumpfile(sql)
      }
      
      case _ => {
        QLog.error(s"ImportSpaceActor called with unknown ImportDataType $importType")
        throw new Exception("Unknown ImportDataType!")
      }
    }    
  }
  
  override def receive = handleChunks orElse {
    case GetProgress => {
      // The client is asking for an update, so calculate where we are:
      if (!uploadComplete) {
        // We arbitrarily count the uploading as the first 20% of the total process:
        val percent = ((uploaded.size / totalSize) * 20).toInt
        sender ! ImportProgress("Uploading...", percent, spaceInfo, failed)
      } else {
        // We're into processing.
        val processPercent = 
          if (thingOps == 0)
            20
          else
            ((thingOps.toFloat / totalThingOps) * 80).toInt + 20
            
        QLog.spew(s"Reporting progress -- we are at $thingOps of $totalThingOps")
            
        sender ! ImportProgress(importMsg, processPercent, spaceInfo, failed)
      }
    }
    
    case CompletionAcknowledged => context.stop(self)
  }
  
  def processBuffer(rc:RequestContext) = {
    if (rc.requester.isEmpty)
      throw new Exception("Somehow got to ImportSpaceActor when not logged in!")
    
    val rawState = buildSpaceState(rc)
    
    loopback(createSpaceFromImported(rc.requesterOrAnon, name)(rawState)) onComplete { 
      case Success(info) => {
        // We don't actually do anything -- we wait for the client to request an update
        // on the situation. So just note that we're done.
        spaceInfo = Some(ClientApi.spaceInfo(info))
      }
      
      case Failure(ex) => {
        failed = true
        QLog.error("Failure while processing uploaded Space", ex)
      }
    }
    
    sender ! UploadProcessSuccessful("Processing in progress")
  }
}

object ImportSpaceActor {
  def actorProps(ecology:Ecology, importType:ImportDataType, name:String, size:Int) = Props(classOf[ImportSpaceActor], ecology, importType, name, size)
  
  case object GetProgress
  
  case object CompletionAcknowledged
}

sealed trait ImportDataType
case object ImportXML extends ImportDataType
case object ImportMySQL extends ImportDataType
