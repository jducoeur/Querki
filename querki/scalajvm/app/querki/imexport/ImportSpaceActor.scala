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
            
        sender ! ImportProgress(importMsg, processPercent, spaceInfo, failed)
      }
    }
    
    case CompletionAcknowledged => context.stop(self)
  }
  
  var importMsg:String = "Uploading"
  
  // How many roundtrips do we expect to make to the DB?
  var totalThingOps = 0
  // How many roundtrips have we made so far?
  var thingOps = 0
  
  // Set when we are complete
  var spaceInfo:Option[querki.data.SpaceInfo] = None
  
  // Set to true iff the upload process fails
  var failed = false
  
  // Needed for buildSpace:
//  def setMsg(msg:String):Unit = importMsg = msg
//  def setTotalThingOps(n:Int) = totalThingOps = n 
//  def incThingOps():Unit = thingOps = thingOps + 1
//  def createMsg:String = "Creating new Space"
//  def buildingMsg:String = "Creating uploader"
//  def thingsMsg:String = "Importing Things into the Space"
//  def typesMsg:String = "Importing Types into the Space"
//  def propsMsg:String = "Importing Properties into the Space"
//  def valuesMsg:String = "Importing Values into the Space"
  
  def startSpaceActor(spaceId:OID):ActorRef = {
    context.actorOf(PersistentSpaceActor.actorProps(ecology, SpacePersistenceFactory, self, spaceId, false), "Space")
  }
  
  def processBuffer(rc:RequestContext) = {
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
        // This just creates the Space in MySQL, but doesn't boot it yet:
        newSpaceId <- createSpace(user, remappedState.id, canon, name, StatusInitializing)
        // Boot the new Space *locally*, so we can throw the State over the wall:
        spaceActor = startSpaceActor(newSpaceId)
        // Slam the State:
        ThingFound(_, finalState) <- spaceActor.request(SetState(user, newSpaceId, remappedState, SetStateReason.ImportedFromExport, rawState.displayName))
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
