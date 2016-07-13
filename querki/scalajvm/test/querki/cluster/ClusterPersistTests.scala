package querki.cluster

import scala.collection.immutable.{HashMap, HashSet}

import akka.actor.ActorPath
import akka.actor.Actor.noSender

import querki.persistence._
import querki.test._

class QuerkiNodeCoordinatorPersistTests(env:PersistEnv) extends PersistTest(env) {
  checkSerialization(QuerkiNodeCoordinator.ShardUnavailable(12))
  checkSerialization(QuerkiNodeCoordinator.ShardAssigned(noSender, 42))
  checkSerialization(QuerkiNodeCoordinator.ShardUnassigned(noSender, 42))
  checkSerialization(
    QuerkiNodeCoordinator.ShardSnapshot(
      HashSet(1, 2), 
      HashMap((ActorPath.fromString("akka.tcp://application@127.0.0.1:2551/system/sharding/UserCache") -> 5))))
}

class OIDAllocationPersistTests(env:PersistEnv) extends PersistTest(env) {
  checkSerialization(OIDAllocator.Alloc(12))
  checkSerialization(OIDAllocator.AllocState(9384847))
}
