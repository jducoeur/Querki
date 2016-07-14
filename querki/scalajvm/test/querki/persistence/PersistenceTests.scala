package querki.persistence

import akka.serialization.Serializer

import org.objenesis.strategy.StdInstantiatorStrategy

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.util.ListReferenceResolver

import com.romix.akka.serialization.kryo.KryoSerializer
import com.romix.scala.serialization.kryo.SubclassResolver

import akka.actor.{ActorSystem, ExtendedActorSystem}

import querki.test.QuerkiTests

trait PersistEnv extends org.scalatest.WordSpecLike {
  def checkObj[T](in:T)
  def checkEquality[T](a:T, b:T)
  // We expose the ActorSystem so that tests can check Akka stuff:
  def testActorSystem:ActorSystem
  def roundtrip[T <: AnyRef](in:T):T
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
 * 
 * Since this whole thing runs in one test, we use one Kryo throughout, minimizing
 * setup costs.
 */
class PersistenceTests 
  extends QuerkiTests with PersistEnv
{
  def checkObj[T](in:T) = assert(in.isInstanceOf[UseKryo])
  def checkEquality[T](a:T, b:T) = assert(a == b)
  
  var _actorSystemOpt:Option[ActorSystem] = None
  def testActorSystem = _actorSystemOpt.get
  
  var _serializer:Option[Serializer] = None
  def serializer = _serializer.get
  def roundtrip[T <: AnyRef](in:T):T = {
    val bytes = serializer.toBinary(in)
    serializer.fromBinary(bytes).asInstanceOf[T]
  }
  
  "Persistence" should {
    "work for all the various packages" in { 
      val actorSystem = ActorSystem().asInstanceOf[ExtendedActorSystem]
      _actorSystemOpt = Some(actorSystem)
      KryoInit.setActorSystem(actorSystem)

      _serializer = Some(new KryoSerializer(actorSystem))
    
      new querki.cluster.QuerkiNodeCoordinatorPersistTests(this)
      new querki.cluster.OIDAllocationPersistTests(this)
      
      _actorSystemOpt = None
      actorSystem.terminate()
    }
  }
}
