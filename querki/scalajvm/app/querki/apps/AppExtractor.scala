package querki.apps

import querki.core.NameUtils
import querki.data.TID
import querki.globals._
import querki.identity.User
import querki.spaces.{PersistentSpaceActor, RTCAble, SpaceCreator, StatusNormal}
import querki.spaces.messages._

trait AppExtractorSupport[RM[_]] {
  def getOIDs(nRequested:Int):RM[Seq[OID]]
  def createSpace(user:User, spaceId:OID, name:String, display:String):RM[OID]
  def setAppState(state:SpaceState):RM[Unit]
  def setChildState(state:SpaceState):RM[Any]
}

/**
 * Deals with extracting Apps. Assumes that it runs inside AppsFunctionsImpl or a reasonable
 * facsimile thereof.
 */
class AppExtractor[RM[_]](state:SpaceState, user:User)(rtcIn:RTCAble[RM], val extractorSupport:AppExtractorSupport[RM])(implicit val ecology:Ecology)  
  extends EcologyMember with ExtracteeComputer with AppRemapper[RM] with Hollower
{
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Apps = interface[Apps]
  
  // These are required by SpacePure, a ways down the stack:
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  lazy val SystemState = System.State
  lazy val id = state.id
  
  implicit val rtc = rtcIn
  private implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  def extractApp(elements:Seq[TID], display:String):RM[Unit] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    val canon = NameUtils.canonicalize(display)
    
    // First, take the list of Things to extract, and turn it into the State of the prospective App...
    val extractees = computeExtractees(elements, display, user)(state)
    val appState = extractees.state
    for {
      // ... take extractees.extractState -- the raw version of the App -- and produce a version of it that
      // has all of its OIDs remapped...
      (remappedApp, idMap) <- remapOIDs(appState, extractees.extractState)
      // ... "hollow" out all of the Things that got extracted up to the App, marking them as Shadows.
      hollowedSpace = hollowSpace(extractees, state, remappedApp, idMap)
      // ... create the App itself...
      appId <- extractorSupport.createSpace(user, remappedApp.id, canon, display)
      // ... update its OID...
      renumberedApp = remappedApp.copy(s = appId)
      // ... set the App's state...
      _ <- extractorSupport.setAppState(renumberedApp)
      // ... and finally, update the child Space to reflect the new reality.
      _ <- extractorSupport.setChildState(hollowedSpace)
    }
      yield ()
  }
}
