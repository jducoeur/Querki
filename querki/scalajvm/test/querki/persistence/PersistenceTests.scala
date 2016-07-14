package querki.persistence

import akka.actor.{ActorSystem, ExtendedActorSystem}

import querki.test.QuerkiTests

trait PersistEnv extends org.scalatest.WordSpecLike {
  def checkObj[T](in:T)
  def checkEquality[T](a:T, b:T)
  def testActorSystem:ActorSystem
}

/**
 * This is the mass runner for all Persistence Tests. The tests in the individual
 * packages should get listed here.
 * 
 * Why do it this way? Because running persistence tests requires a fully-configured
 * Kryo, which requires a KryoInit, which (a) is static and (b) requires setting up
 * an ActorSystem, which is very expensive and potentially conflicting. So we don't
 * want a lot of persistence tests running in parallel, we want to do them all as a
 * single suite, with a single setup cost.
 */
class PersistenceTests 
  extends QuerkiTests with PersistEnv
{
  def checkObj[T](in:T) = assert(in.isInstanceOf[UseKryo])
  def checkEquality[T](a:T, b:T) = assert(a == b)
  
  var _actorSystemOpt:Option[ActorSystem] = None
  def testActorSystem = _actorSystemOpt.get
  
  "Persistence" should {
    "work for all the various packages" in { 
      val actorSystem = ActorSystem().asInstanceOf[ExtendedActorSystem]
      _actorSystemOpt = Some(actorSystem)
      KryoInit.setActorSystem(actorSystem)
    
      new querki.cluster.QuerkiNodeCoordinatorPersistTests(this)
      new querki.cluster.OIDAllocationPersistTests(this)
      
      _actorSystemOpt = None
      actorSystem.terminate()
    }
  }
}
