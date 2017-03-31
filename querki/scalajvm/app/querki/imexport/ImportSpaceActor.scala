package querki.imexport

import scala.util.{Failure, Success}

import akka.actor._

import upickle._

import org.querki.requester._

import models._
import querki.cluster.OIDAllocator._
import querki.globals._
import ImportSpaceFunctions._
import querki.history.HistoryFunctions._
import querki.spaces._
import querki.spaces.messages._
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
class ImportSpaceActor(e:Ecology, importType:ImportDataType, name:String, totalSize:Int) extends Actor with Requester 
  with UploadActor with Remapper[RequestM] with SpaceCreator with querki.core.NameUtils with EcologyMember 
{
  import ImportSpaceActor._
  
  implicit val ecology = e
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Cluster = interface[querki.cluster.QuerkiCluster]
  lazy val Core = interface[querki.core.Core]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val SpacePersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  
  implicit def rtc = RealRTCAble
  def getOIDs(nRequested:Int):RequestM[Seq[OID]] = {
    Cluster.oidAllocator.requestFor[NewOIDs](GiveOIDBlock(nRequested)).map(_.oids)
  }
  lazy val persister = SpacePersistenceFactory.getSpaceManagerPersister(context)
  
  def buildSpaceState(rc:RequestContext):SpaceState = {
    importType match {
      case ImportXML => {
        val xml = uploaded
        new RawXMLImport(rc)(ecology).readXML(xml)
      }
      
      case ImportMySQL => {
        val sql = uploaded
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
        // We arbitrarily count the uploading as the first 50% of the total process:
        val percent = ((uploaded.size / totalSize) * 50).toInt
        sender ! ImportProgress("Uploading...", percent, spaceInfo, failed)
      } else {
        // We're into processing.
        sender ! ImportProgress("Processing...", processPercent, spaceInfo, failed)
      }
    }
    
    case CompletionAcknowledged => context.stop(self)
  }
  
  var importMsg:String = "Uploading"
  
  var processPercent:Int = 0
  
  // Set when we are complete
  var spaceInfo:Option[querki.data.SpaceInfo] = None
  
  // Set to true iff the upload process fails
  var failed = false
  
  def startSpaceActor(spaceId:OID):ActorRef = {
    context.actorOf(PersistentSpaceActor.actorProps(ecology, SpacePersistenceFactory, self, spaceId, false), "Space")
  }
  
  def processBuffer(rc:RequestContext) = {
    processPercent = 50
    
    if (rc.requester.isEmpty)
      throw new Exception("Somehow got to ImportSpaceActor when not logged in!")
    
    def handleError(ex:Throwable) = {
      importMsg = ex match {
        case pex:PublicException => pex.display(Some(rc))
        case _ => "There was an error trying to upload that Space -- sorry! Please contact us, so we can look into it."
      }
      
      failed = true      
    }
    
    try {
      // Given the XML or MySQL file, build a SpaceState:
      val rawState = buildSpaceState(rc)
      processPercent = 60
      val canon = canonicalize(name)
      val renamedState = rawState.copy(
        name = canon,
        pf = rawState.props ++
          toProps(
            Core.NameProp(canon),
            Basic.DisplayNameProp(name)
          ))
      val user = rc.requesterOrAnon
      
      for {
        // Remap all of the OIDs in the SpaceState to use new ones:
        (remappedState, oidMap) <- remapOIDs(renamedState)
        _ = processPercent = 70
        // This just creates the Space in MySQL, but doesn't boot it yet:
        newSpaceId <- createSpace(user, remappedState.id, canon, name, StatusInitializing)
        _ = processPercent = 80
        // Boot the new Space *locally*, so we can throw the State over the wall:
        spaceActor = startSpaceActor(newSpaceId)
        // Slam the State:
        ThingFound(_, finalState) <- spaceActor.request(SetState(user, newSpaceId, remappedState, SetStateReason.ImportedFromExport, rawState.displayName))
        _ = processPercent = 90
        // Finally, once it's all built, shut down this temp Actor:
        _ = context.stop(spaceActor)
        // And tell the SpaceManager that this Space is now ready to be treated normally:
        _ <- SpaceOps.spaceManager.requestFor[StatusChanged.type](ChangeSpaceStatus(newSpaceId, StatusNormal))
      }
      {
        // We don't actually do anything -- we wait for the client to request an update
        // on the situation. So just note that we're done.
        spaceInfo = Some(ClientApi.spaceInfo(remappedState, user))
      }
    } catch {
      // Caught an error while trying to build the SpaceState:
      case ex:Throwable => handleError(ex)
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
