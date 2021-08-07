package querki.system

import akka.actor._

import querki.ecology._
import querki.globals._
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

  var ecology: EcologyImpl = null

  var initCaller: Option[ActorRef] = None

  def createActor(
    props: Props,
    name: String
  ): Option[ActorRef] = Some(context.actorOf(props, name))

  def checkAsyncInitDone() = {
    if (ecology.waitingForAsync) {
      QLog.spew(s"Waiting for ${ecology.asyncInitters} to initialize")
    } else {
      initCaller.map { origSender =>
        QLog.spew("... Querki running.")
        origSender ! Initialized(ecology)
      }
    }
  }

  def receive = {
    case Initialize(app) => {
      println("Creating the Ecology...")
      val ecologyImpl = new EcologyImpl(Some(app))
      ecology = ecologyImpl
      SystemCreator.createAllEcots(ecology, Some(context.system), self)
      println("... initializing the Ecology...")
      val finalState = ecologyImpl.init(InitialSystemState.create(ecology), createActor)

      // Allow all the systems to cache their stuff into System:
      val withCache =
        ecology.api[querki.spaces.SpaceChangeManager].updateStateCache(CacheUpdate(None, None, finalState))
      ecology.api[SystemManagement].setState(withCache.current)

      initCaller = Some(sender)

      // Wait until any async initters are complete:
      checkAsyncInitDone()
    }

    case AsyncInitted(clazz) => {
      QLog.spew(s"Async Initter $clazz is ready")
      ecology.gotAsync(clazz)
      checkAsyncInitDone()
    }

    case Terminate => {
      ecology.manager.term

      sender ! Terminated
    }
  }
}

object QuerkiRoot {
  case class Initialize(app: play.api.Application)
  case class Initialized(ecology: Ecology)
  case object Terminate
  case object Terminated

  // DO NOT USE THIS UNLESS YOU KNOW WHAT YOU ARE DOING!!!
  // This cheat exists solely to allow the functional tests to get at the real Ecology. It
  // should *never* be used for any other purpose, including unit tests. (Which run in parallel,
  // and have their own individual Ecologies.)
  var ecology: Ecology = null
}
