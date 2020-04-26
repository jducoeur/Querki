package querki.publication

import akka.actor.{ActorRef, Props}
import akka.persistence._
import funcakka._
import org.querki.requester._
import models._
import querki.globals._
import querki.identity.{IdentityId, User, PublicIdentity}
import querki.spaces.TracingSpace
import querki.spaces.messages._

class PublicationActor(val ecology:Ecology, val id:OID, val router:ActorRef) 
  extends PublicationCore with RealActorCore with PersistentActor with Requester with EcologyMember 
{
  /**
   * TODO: enhance PersistentActorCore to include a version of request(), so this can run completely in
   * PublicationCore.
   */
  def sendChangeProps(who:User, pairs:Seq[(OID, PropMap)])(implicit state:SpaceState):RequestM[SpaceState] = {
    (RequestM.successful(state) /: pairs) { case (rm, (thingId, propMap)) =>
      for {
        _ <- rm
        ThingFound(_, nextState) <- router.request(ChangeProps(who, state.id, thingId, propMap))
      }
        yield nextState
    }
  }
}

object PublicationActor {
  def actorProps(ecology:Ecology, spaceId:OID, router:ActorRef) = Props(classOf[PublicationActor], ecology, spaceId, router)
}