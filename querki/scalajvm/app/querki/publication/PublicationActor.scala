package querki.publication

import akka.actor.ActorRef
import akka.persistence._

import funcakka._
import org.querki.requester._

import models._
import querki.globals._
import querki.identity.{IdentityId, PublicIdentity, User}
import querki.spaces.messages._

class PublicationActor(val ecology:Ecology, val id:OID, val router:ActorRef) 
  extends PublicationCore with RealActorCore with PersistentActor with Requester with EcologyMember 
{
  /**
   * TODO: enhance PersistentActorCore to include a version of request(), so this can run completely in
   * PublicationCore.
   */
  def sendPermissionUpdates(who:User, pairs:Seq[(OID, PropMap)])(implicit state:SpaceState):RequestM[Unit] = {
    (RequestM.successful(()) /: pairs) { case (rm, (thingId, propMap)) =>
      rm.flatMap(_ => router.request(ChangeProps(who, state.id, thingId, propMap)).map(_ => ()))
    }
  }
}
