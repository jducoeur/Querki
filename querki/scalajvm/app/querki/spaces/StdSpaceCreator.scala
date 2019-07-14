package querki.spaces

import akka.actor._
import org.querki.requester._
import querki.ecology.SystemIds
import querki.globals._
import querki.identity
import querki.spaces.PersistMessages._
import querki.spaces.messages._
import querki.values.SpaceState

/**
 * This special Actor is designed to be booted up as a System Singleton at startup. It does nothing
 * but ensure that the specified "system" Space exists.
 * 
 * TODO: This is a fairly stupid way of dealing with this: it leaves System Singletons sitting around, and will
 * get re-run every time the Singleton gets rebalanced, and it is at least theoretically susceptible to startup
 * race conditions, since system init doesn't wait for a result here. Come up with a better approach. 
 */
private [spaces] class StdSpaceCreator(val spaceId:OID, val name:String, val display:String, val ecology:Ecology) 
  extends Actor with Requester with EcologyMember 
{
  lazy val TimeProvider = interface[querki.time.TimeProvider]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val SpaceOps = interface[SpaceOps]  
  lazy val SpacePersistenceFactory = interface[SpacePersistenceFactory]
  lazy val System = interface[querki.system.System]
  
  lazy val persister = SpacePersistenceFactory.getSpaceManagerPersister
  
  private case class Boot()
  
  override def preStart() = {
    self ! Boot()
    super.preStart()
  }
  
  def receive = {
    case Boot() => {
      (persister ? CreateSpaceIfMissing(querki.identity.MOIDs.SystemIdentityOID, spaceId, 10000, name, display, StatusNormal)) foreach {
        case Changed(_, _) => {
          val emptyState = SpaceState(
            spaceId,
            SystemIds.systemOID,
            models.emptyProps,
            querki.identity.MOIDs.SystemIdentityOID,
            display,
            TimeProvider.now,
            Seq.empty,
            Some(System.State),
            Map.empty,
            Map.empty,
            Map.empty,
            Map.empty,
            None
          )
          // Okay, we actually needed to create the Space, so now we should initialize it. This apes code in SpaceManager.
          // Note that we're just doing a tell, though -- we don't need to do anything with the response:
          SpaceOps.spaceRegion ! InitialState(IdentityAccess.systemRequestContext(emptyState), spaceId, display, querki.identity.MOIDs.SystemIdentityOID)
        }
        case NoChangeNeeded(_) => // Normal case -- we're all set
      }
    }
  }
}

object StdSpaceCreator {
  def actorProps(spaceId:OID, name:String, display:String, ecology:Ecology) = Props(classOf[StdSpaceCreator], spaceId, name, display, ecology)
}
