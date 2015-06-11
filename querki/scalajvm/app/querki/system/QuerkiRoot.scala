package querki.system

import akka.actor._

import querki.ecology._

import querki.spaces.{CacheUpdate, DBSpacePersistenceFactory, SpaceManager, SpaceOps}

/**
 * The master root for all of Querki's back end. Note that this does *not* control the front end
 * actors (which are owned by Play), but the entire Space system, as well as anything else *not*
 * directly owned by Play, should be supervised here.
 * 
 * For the time being, this is basically nothing but a root supervisor -- it has no real messages of
 * its own. This may well change eventually, especially once we get into clustering.
 */
class QuerkiRoot extends Actor {
  
  import QuerkiRoot._
  
  def ecology = Ecology.ecology
  
  def createActor(props:Props, name:String):Option[ActorRef] = Some(context.actorOf(props, name))
  
  def receive = {
    case Initialize => {
      println("Creating the Ecology...")
      val ecologyImpl = new EcologyImpl
      Ecology.setEcology(ecologyImpl)
      SystemCreator.createAllEcots(ecology, Some(context.system))
      println("... initializing the Ecology...")
      val finalState = ecologyImpl.init(InitialSystemState.create(ecology), createActor)
      
      // Allow all the systems to cache their stuff into System:
      val withCache = ecology.api[querki.spaces.SpaceChangeManager].updateStateCache(CacheUpdate(None, None, finalState))
      ecology.api[SystemManagement].setState(withCache.current)
      
      println("... Querki running.")
      sender ! Initialized(ecology)
    }
    
    case Terminate => {
      ecology.manager.term
      
      sender ! Terminated
    }
  }
}

object QuerkiRoot {
  case object Initialize
  case class Initialized(ecology:Ecology)
  case object Terminate
  case object Terminated
}
