package querki.system

import akka.actor._

import querki.ecology._

import querki.spaces.{DBSpacePersistenceFactory, SpaceManager}

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
  
  def receive = {
    case Initialize => {
      SystemCreator.createAllEcots(ecology)
      val finalState = ecology.manager.init(models.system.SystemSpace.initialSystemState)
      ecology.api[SystemManagement].setState(finalState)
    
      // Note that the SpacePersistenceFactory is intentionally defined all the way up here. That is
      // specifically for testing, so that we can stub it out and replace it with a mock version.
      // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
      // with "Props(classOf(Space), ...)". See:
      //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
      val ref = context.actorOf(Props(new SpaceManager(new DBSpacePersistenceFactory)), name="SpaceManager")
      SpaceManager.setSpaceManager(ref)
      
      sender ! Initialized
    }
    
    case Terminate => {
      ecology.manager.term
      
      sender ! Terminated
    }
  }
}

object QuerkiRoot {
  case object Initialize
  case object Initialized
  case object Terminate
  case object Terminated
}
