package querki.persistence

import akka.serialization.Serializer

import org.objenesis.strategy.StdInstantiatorStrategy

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.util.ListReferenceResolver

import com.romix.akka.serialization.kryo.KryoSerializer
import com.romix.scala.serialization.kryo.SubclassResolver

import akka.actor.{ActorSystem, ExtendedActorSystem}

import querki.globals._
import querki.identity.User
import querki.test._

trait PersistEnv extends org.scalatest.WordSpecLike with EcologyMember {
  def asrt(a:Boolean)
  def checkEquality[T](a:T, b:T)
  // We expose the ActorSystem so that tests can check Akka stuff:
  def testActorSystem:ActorSystem
  def roundtrip[T <: AnyRef](in:T):T
  
  def commonSpace:CommonSpace
  def cdSpace:CDSpace
  // Exposes the standard pql() construct in a way that PersistTests can use it:
  def pqlEquals[S <: TestSpace](text:String, expected:String)(implicit space:S, requester:User = BasicTestUser)
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
  def asrt(a:Boolean) = assert(a)
  def checkEquality[T](a:T, b:T) = assert(a == b)
  
  def pqlEquals[S <: TestSpace](text:String, expected:String)(implicit space:S, requester:User = BasicTestUser) = {
    pql(text) should equal (expected)
  }
  
  var _actorSystemOpt:Option[ActorSystem] = None
  def testActorSystem = _actorSystemOpt.get
  
  var _serializer:Option[Serializer] = None
  def serializer = _serializer.get
  def roundtrip[T <: AnyRef](in:T):T = {
    val bytes = serializer.toBinary(in)
    serializer.fromBinary(bytes).asInstanceOf[T]
  }
  
  var _cdSpace:Option[CDSpace] = None
  def cdSpace = _cdSpace.get
  
  "Persistence" should {
    "work for all the various packages" in {
      _cdSpace = Some(new CDSpace)
      
      val actorSystem = ActorSystem().asInstanceOf[ExtendedActorSystem]
      _actorSystemOpt = Some(actorSystem)
      KryoInit.setActorSystem(actorSystem)

      _serializer = Some(new KryoSerializer(actorSystem))
    
      new CommonPersistenceTests(this)
      new models.ModelPersistenceTests(this)
      new querki.apps.AppPersistenceTests(this)
      new querki.cluster.QuerkiNodeCoordinatorPersistTests(this)
      new querki.cluster.OIDAllocationPersistTests(this)
      new querki.conversations.ConversationEventPersistenceTests(this)
      new querki.spaces.SpaceMessagePersistenceTests(this)
      
      _actorSystemOpt = None
      actorSystem.terminate()
    }
  }
}
