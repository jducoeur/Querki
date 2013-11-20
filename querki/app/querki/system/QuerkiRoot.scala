package querki.system

import akka.actor._

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
  
  def receive = {
    case Initialize => {
      // Yes, it's a static initialization function. For now, it seems to be fine, because SystemSpace
      // is *quite* explicitly and deliberately immutable. So we create it now, and it's available
      // to everyone after that.
      models.system.SystemSpace.init
    
      // Note that the SpacePersistenceFactory is intentionally defined all the way up here. That is
      // specifically for testing, so that we can stub it out and replace it with a mock version.
      // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
      // with "Props(classOf(Space), ...)". See:
      //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
      val ref = context.actorOf(Props(new SpaceManager(new DBSpacePersistenceFactory)), name="SpaceManager")
      SpaceManager.setSpaceManager(ref)
      
      sender ! Initialized
    }
  }
}

object QuerkiRoot {
  case object Initialize
  case object Initialized
}
