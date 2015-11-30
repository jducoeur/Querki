package querki.apps

import akka.actor._

import org.querki.requester._

import models._

import querki.api.ProgressActor
import querki.data.TID
import querki.globals._
import querki.identity.User
import querki.spaces.SpaceBuilder
import querki.spaces.messages._
import querki.util.QuerkiActor
import querki.values.SpaceState

/**
 * The Actor that shepherds the process of extracting a new App from an existing Space. This is separate
 * from the Space's main troupe, so that it can reboot the Space when needed.
 * 
 * Note that this is a ProgressActor, and follows that pattern of operation.
 * 
 * @author jducoeur
 */
private [apps] class ExtractAppActor(val ecology:Ecology, val elements:Seq[TID], val name:String, val owner:User, val state:SpaceState, val router:ActorRef) 
  extends Actor with Requester with EcologyMember with ProgressActor with SpaceBuilder with ExtracteeComputer with Hollower
{
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  implicit val s = state
  implicit val ec = ecology  
  
  override def receive = handleRequestResponse orElse handleProgress

  def calcProgress():Int = {
    currentState.soFar
  }
  
  trait ProcessState {
    def soFar:Int
  }
  class PSC(val soFar:Int) extends ProcessState
  case object BackingUp extends PSC(10)
  case object ComputingExtractees extends PSC(20)
  case object BuildingSpace extends ProcessState {
    def soFar = 20 + (30 * spaceBuilderPct).toInt
  }
  case object Reloading extends PSC(60)
  // TODO: this should become more nuanced, like BuildingSpace:
  case object Hollowing extends PSC(90)
  case object Finished extends PSC(100)
  
  var currentState:ProcessState = BackingUp
  def setState(state:ProcessState) = {
    currentState = state
  }
  
  def withMsg[R](msg:String, f: => R):R = {
    setMsg(msg)
    f
  } 
  
  def doWork():Unit = {
//    // TEMP:
//    QLog.spew("In ExtractAppActor:")
//    elements foreach { elementId =>
//      state.anything(ThingId(elementId.underlying)) match {
//        case Some(element) => QLog.spewThing(element)(state)
//        case _ => QLog.error(s"Unknown TID $elementId!")
//      }
//    }
    
    withMsg("Backing up the Space", backupSpace())
    
    setState(ComputingExtractees)
    
    val extractees = withMsg("Figuring out everything to extract", computeExtractees())
    
    setState(BuildingSpace)
    
    for {
      // This builds a new Space with new OIDs, based on the partial SpaceState in the Extractees:
      newSpaceInfo <- loopback(buildSpace(owner, name)(extractees.state))
      dummy1 = setState(Reloading)
      // This will return once the reload process has *started*. It may take a while before the Space is
      // fully reloaded, but it should stash until then:
      ThingFound(_, _) <- withMsg("Reloading Space with new App", router ? SpacePluginMsg(owner, state.id, AddApp(newSpaceInfo.info.id)))
      dummy2 = setState(Hollowing)
      dummy <- hollowSpace(extractees, newSpaceInfo)
    }
      setState(Finished)
  }
  
  ///////////////////////////////
  //
  // Step 1: Backup
  //
  
  import querki.db._
  /**
   * Before we start, do a backup to be on the safe side.
   * 
   * TODO: in principle, this should be extracted out to another Actor. In practice, it can
   * go away once we move to Akka Persistence anyway, so we're not going to worry about it too much.
   */
  def backupSpace() = {
    QDB(ShardKind.User) { implicit conn =>
      SpacePersistence.SpaceSQL(state.id, """
          CREATE TABLE {bname} LIKE {tname}
          """).executeUpdate
      SpacePersistence.SpaceSQL(state.id, """
          INSERT {bname} SELECT * FROM {tname}
          """).executeUpdate      
    }
  }
  
  ////////////////////////////////
  //
  // Step 3: Actually create the App
  //
  // All the actual work is done in SpaceBuilder's buildSpace(); these are its requirements:
  //
  
  var totalThingOps:Int = 0
  var thingOps:Int = 0
  
  def spaceBuilderPct = {
    if (totalThingOps == 0)
      0
    else
      (thingOps.toFloat / totalThingOps.toFloat)
  }
  
  def setMsg(msg:String):Unit = phaseDescription = msg
  def setTotalThingOps(n:Int) = totalThingOps = n 
  def incThingOps():Unit = thingOps = thingOps + 1
  def createMsg:String = "Creating the App"
  def buildingMsg:String = "Preparing to extract to the App"
  def thingsMsg:String = "Copying Things into the App"
  def typesMsg:String = "Copying Types into the App"
  def propsMsg:String = "Copying Properties into the App"
  def valuesMsg:String = "Copying Values into the App"

  ///////////////////////////////
  //
  // Step 4: Hollow out the Space
  //
  // Now that we have an App, we need to remove the redundant bits from the Space, so that it is just
  // using the App's copy. By and large, the extracted elements in the Space wind up as shadows of the
  // versions in the App. We retain these shadows so that we can make local customizations in the Space.
  //
}

object ExtractAppActor {
  def props(e:Ecology, elements:Seq[TID], name:String, owner:User, state:SpaceState, router:ActorRef) = 
    Props(classOf[ExtractAppActor], e, elements, name, owner, state, router) 
}
