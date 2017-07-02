package querki.persistence

import akka.actor.{ActorPath, ActorRef, ExtendedActorSystem}

import com.esotericsoftware.kryo._
import com.romix.scala.serialization.kryo._
import com.romix.akka.serialization.kryo._

import querki.globals._

/**
 * This class gets plugged in via config, in:
 * 
 *   akka.actor.kryo.kryo-custom-serializer-init
 *   
 * It tells Kryo to use the TaggedFieldSerializer, so that persisted types can evolve properly.
 * 
 * IMPORTANT: the implication here is that only fields that are marked with @Tag(int) will be
 * serialized! We are demanding some serious discipline in our persisted types, so that we
 * can have schema evolution without too much pain. 
 * 
 * TODO: can we make this injected, to make it less statically horrible?
 */
class KryoInit {
  def customize(kryo:Kryo):Unit = {
    
//    QLog.spew(s"Customizing a Kryo instance...")
    
    // First, register the standard Scala and Akka types that we sometimes need. Note that these
    // are all being registered as, effectively, EcotId 0:
    KryoInit.registerAkkaMsgs(kryo)
    
    // Then, register the actual application messages, which have been declared in 
    // their Ecots, so their ids reflect the Ecot they come from:
    KryoInit.registerMsgs(kryo)
//    QLog.spew(s"... done customizing.")
    
    // And store away this Kryo, in case we're not yet done with initialization:
    KryoInit._rawKryos = KryoInit._rawKryos :+ kryo
    
    // TODO: once Kryo 3.0.4 is released, figure out how to leverage
    // TaggedFieldSerializer.setIgnoreUnknownTags(true) -- this should give
    // us rudimentary forward compatibility. That probably doesn't matter
    // much for persistence, but will be useful if we ever use Kryo for network
    // serialization in a rolling-upgrades scenario.
  }
}

/**
 * Static kludges to make Kryo initialization work.
 * 
 * TODO: this is all horrible. We might need to rework Ecology setup to make it suck less. Could we
 * *create* the Ecots earlier in QuerkiApplicationLoader, register the messages, and then later
 * deal with Ecot initialization? Basically, we're looking for a way to do all of this setup before
 * the first Kryo gets created.
 */
object KryoInit {
  /**
   * Our own horrible registry of the Kryo pool.
   * 
   * TODO: is it possible for us to access the actual KryoSerializer.serializerPool?
   */
  var _rawKryos:Seq[Kryo] = Seq.empty
  /**
   * The messages declared by the various Ecots, which need to get registered into the Kryos.
   */
  var _msgDecls:Option[Seq[(Class[_], Int)]] = None
  /**
   * The ActorSystem that we're operating in.
   */
  var _actorSystem:Option[ExtendedActorSystem] = None
  
  /**
   * Tells the Kryo system about the messages contained in the Ecology.
   */
  def setMsgs(msgs:Seq[(Class[_], Int)]):Unit = {
    // Unfortunately, we currently have to allow this to be called repeatedly, for unit tests.
    // This is pretty much an indictment of this general architecture -- we desperately want
    // something better.
    if (_msgDecls.isEmpty) {    
      _msgDecls = Some(msgs)
      
      // If there are existing Kryos, bring them up to date.
      // TODO: this is dangerous from a threading POV! Can we do something smarter?
      _rawKryos.foreach { registerMsgs(_) }
    }
  }
  
  def setActorSystem(system:ExtendedActorSystem):Unit = {
    if (_actorSystem.isEmpty) {
      _actorSystem = Some(system)
      
      _rawKryos.foreach { registerAkkaMsgs(_) }
    }
  }
  
  /**
   * Register our own core types, and some from Akka and Scala.
   * 
   * IMPORTANT: many of these types are abstract, and rely upon the new SubclassResolver that we've
   * added to the romix library.
   */
  def registerAkkaMsgs(kryo:Kryo):Unit = {
    _actorSystem.map { actorSystem =>
      kryo.register(classOf[ActorPath], new ActorPathSerializer(actorSystem), 100)
      kryo.register(classOf[ActorRef], new ActorRefSerializer(actorSystem), 101)
      
      kryo.register(classOf[models.OID], new OIDSerializer, 102)
      
      kryo.register(classOf[scala.collection.immutable.Set[_]], new ScalaImmutableAbstractSetSerializer, 103)
      kryo.register(classOf[scala.collection.immutable.Map[_, _]], new ScalaImmutableAbstractMapSerializer, 104)
      
      kryo.register(classOf[models.ThingId], new ThingIdSerializer, 105)

      // We register the two halves of Option separately, since we really want to serialize them
      // differently:
      kryo.register(None.getClass(), new NoneSerializer, 106)
      kryo.register(classOf[Some[_]], new SomeSerializer, 107)
      
      kryo.register(classOf[querki.time.DateTime], new DateTimeSerializer, 108)
      
      kryo.register(classOf[scala.collection.immutable.List[_]], new ScalaListSerializer, 109)
      
      kryo.register(classOf[scala.Tuple1[_]], new ScalaProductSerializer(kryo), 110)
      kryo.register(classOf[scala.Tuple2[_, _]], new ScalaProductSerializer(kryo), 111)
      kryo.register(classOf[scala.Tuple3[_, _, _]], new ScalaProductSerializer(kryo), 112)
      kryo.register(classOf[scala.Tuple4[_, _, _, _]], new ScalaProductSerializer(kryo), 113)
      
      kryo.register(classOf[scala.collection.immutable.Stream[_]], new ScalaStreamSerializer, 114)
      
      kryo.register(classOf[AddedField[_]], new AddedFieldSerializer, 115)
      
      kryo.register(classOf[scala.collection.immutable.Vector[_]], new ScalaVectorSerializer, 116)
    }
  }
  
  /**
   * Register the messages from the Ecology into this Kryo.
   */
  def registerMsgs(kryo:Kryo):Unit = {
    _msgDecls.foreach { decls => 
//      QLog.spew(s"... registering message declarations...")
      decls.foreach { case (clazz, id) =>
//        QLog.spew(s"    ${clazz.getCanonicalName} = $id")
        kryo.register(clazz, new serializers.TaggedFieldSerializer(kryo, clazz), id)
      }
    }    
  }
}
