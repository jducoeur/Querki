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
  val system = env.testActorSystem
  val testActor = system.actorOf(Props(classOf[TestActor]))
  val defaultAddress = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
  val testPath = QuerkiNodeCoordinator.NodePath(testActor.path.toStringWithAddress(defaultAddress))
  
  checkSerialization(QuerkiNodeCoordinator.ShardUnavailable(12))
  checkSerialization(QuerkiNodeCoordinator.ShardAssigned(testPath, 42))
  checkSerialization(QuerkiNodeCoordinator.ShardUnassigned(testPath, 42))
  checkSerialization(
    QuerkiNodeCoordinator.ShardSnapshot(
      HashSet(1, 2), 
      HashMap((testPath -> 5))))
}

class OIDAllocationPersistTests(env:PersistEnv) extends PersistTest(env) {
  checkSerialization(OIDAllocator.Alloc(12))
  checkSerialization(OIDAllocator.AllocState(9384847))
}
