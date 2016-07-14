package querki.cluster

import scala.collection.immutable.{HashMap, HashSet}

import akka.actor._
import akka.actor.Actor.noSender

import querki.globals._
import querki.persistence._
import querki.test._

class TestActor extends Actor {
  def receive = {
    case something => QLog.spew(s"TestActor got $something")
  }
}

class QuerkiNodeCoordinatorPersistTests(env:PersistEnv) extends PersistTest(env) {
  val testActor = env.testActorSystem.actorOf(Props(classOf[TestActor]))
  
  checkSerialization(QuerkiNodeCoordinator.ShardUnavailable(12))
  checkSerialization(QuerkiNodeCoordinator.ShardAssigned(testActor, 42))
  checkSerialization(QuerkiNodeCoordinator.ShardUnassigned(testActor, 42))
  checkSerialization(
    QuerkiNodeCoordinator.ShardSnapshot(
      HashSet(1, 2), 
      HashMap((ActorPath.fromString("akka.tcp://application@127.0.0.1:2551/system/sharding/UserCache") -> 5))))
}

class OIDAllocationPersistTests(env:PersistEnv) extends PersistTest(env) {
  checkSerialization(OIDAllocator.Alloc(12))
  checkSerialization(OIDAllocator.AllocState(9384847))
}
