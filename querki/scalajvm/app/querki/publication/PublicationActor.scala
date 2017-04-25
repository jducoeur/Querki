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
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
  // TODO: figure out how to drag the semantics of this higher up the stack. The immediate
  // issue is that IdentityAccess.getIdentities() is written in terms of Future, and we
  // need RequestM; neither concept exists in PublicationCore. How do we abstract this
  // appropriately? I suspect that the MonadError may start bleeding into the Ecology level.
  def getIdentities(identityIds:Seq[IdentityId]):RequestM[Map[OID, PublicIdentity]] = {
    loopback(IdentityAccess.getIdentities(identityIds.toSeq))
  }
  
  def sendPermissionUpdates(who:User, pairs:Seq[(OID, PropMap)])(implicit state:SpaceState):RequestM[Unit] = {
    (RequestM.successful(()) /: pairs) { case (rm, (thingId, propMap)) =>
      rm.flatMap(_ => router.request(ChangeProps(who, state.id, thingId, propMap)).map(_ => ()))
    }
  }
}
