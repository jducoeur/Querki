package querki.apps

import models.Kind
import models.Thing.PropMap
import querki.core.NameUtils
import querki.data.TID
import querki.globals._
import querki.history.HistoryFunctions.SetStateReason
import querki.identity.User
import querki.spaces.{PersistentSpaceActor, RTCAble, SpaceCreator, StatusNormal}
import querki.spaces.messages._

trait AppExtractorSupport[RM[_]] {
  def getOIDs(nRequested:Int):RM[Seq[OID]]
  def createSpace(user:User, spaceId:OID, name:String, display:String):RM[OID]
  // Sets the State of the App (identified by state.id). Returns the State of the App after saving.
  def setAppState(state:SpaceState):RM[SpaceState]
  // Sends the given message to the target Space. The Space should reply with ThingAck, and this
  // returns the OID of the relevant Thing. Note that this requires non-localCall messages!
  def sendSpaceMessage(msg:SpaceMessage):RM[OID]
  // Sends the given message to *this* Space. This allows any message.
  def sendMessageToSelf(msg:SpaceMessage):RM[Any]
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
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
  // These are required by SpacePure, a ways down the stack:
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  lazy val SystemState = System.State
  lazy val id = state.id
  
  implicit val rtc = rtcIn
  private implicit def rm2rtc[A](rm:RM[A]) = rtc.toRTC(rm)
  
  def extractApp(elements:Seq[TID], display:String, summary:String, details:String):RM[SpaceState] = {
    if (!AccessControl.hasPermission(Apps.CanManipulateAppsPerm, state, user, state))
      throw new PublicException("Apps.notAllowed")
    
    val canon = NameUtils.canonicalize(display)
    
    // First, take the list of Things to extract, and turn it into the State of the prospective App...
    val extractees = computeExtractees(elements, display, canon, user)(state)
    val appState = extractees.state
    for {
      // ... take extractees.extractState -- the raw version of the App -- and produce a version of it that
      // has all of its OIDs remapped...
      (remappedApp, idMap) <- remapOIDs(appState, extractees.extractState)
      // ... "hollow" out all of the Things that got extracted up to the App, marking them as Shadows.
      hollowedSpace = hollowSpace(extractees, state, remappedApp, idMap)
      // ... create the App itself...
      appId <- extractorSupport.createSpace(user, remappedApp.id, canon, display)
      // ... update its OID and other props...
      renumberedApp = 
        remappedApp.copy(
          s = appId,
          pf = 
            remappedApp.props
            + Apps.GallerySummary(summary)
            + Apps.GalleryDetails(details)
            + Apps.IsAppFlag(true))
      // ... add the App to the Gallery...
      props = 
        Map(
          Apps.GalleryAppId(renumberedApp.id),
          Basic.DisplayNameProp(display),
          Apps.GalleryOwner(renumberedApp.owner),
          Apps.GallerySummary(summary),
          Apps.GalleryDetails(details)
        )
      galleryId <- extractorSupport.sendSpaceMessage(CreateThing(IdentityAccess.SystemUser, MOIDs.GallerySpaceOID, Kind.Thing, MOIDs.GalleryEntryModelOID, props, localCall = false))
      // ... record the entry ID of this App, so we can get back to it...
      appWithGallery = renumberedApp.copy(pf = renumberedApp.props + (Apps.GalleryEntryId(galleryId)))
      // ... set the App's state...
      finalAppState <- extractorSupport.setAppState(appWithGallery)
      // ... update the child Space to reflect the new reality...
      _ <- extractorSupport.sendMessageToSelf(SetState(user, hollowedSpace.id, hollowedSpace, SetStateReason.ExtractedAppFromHere, display))
      // ... and add the App to the child Space.
      _ <- extractorSupport.sendMessageToSelf(SpacePluginMsg(user, hollowedSpace.id, AddApp(finalAppState.id, finalAppState.version, true)))
    }
      yield hollowedSpace
  }
}
