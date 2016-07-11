package querki.persistence

import akka.actor.{ChildActorPath, ExtendedActorSystem}

import com.esotericsoftware.kryo._
import com.romix.scala.serialization.kryo._

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
 */
class KryoInit {
  def customize(kryo:Kryo):Unit = {
    
    QLog.spew(s"Customizing a Kryo instance...")
    
    // First, register the standard Scala and Akka types that we sometimes need. Note that these
    // are all being registered as, effectively, EcotId 0:
    KryoInit.registerAkkaMsgs(kryo)
    
    // Then, register the actual application messages, which have been declared in 
    // their Ecots, so their ids reflect the Ecot they come from:
    KryoInit.registerMsgs(kryo)
    QLog.spew(s"... done customizing.")
    
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
   * Register our own core types, and some from Akka.
   */
  def registerAkkaMsgs(kryo:Kryo):Unit = {
    _actorSystem.map { actorSystem =>
      akka.actor.AkkaHack.setupPrivateSerializers(kryo, actorSystem)
      kryo.register(classOf[ChildActorPath], new ChildActorPathSerializer(actorSystem), 100)
      // 101 -- AkkaHack: LocalActorRef
      kryo.register(classOf[models.OID], new OIDSerializer, 102)
      
      // Annoyingly, we apparently have to register these special cases individually. Kryo doesn't appear to
      // be smart enough to deal with subclasses, even if the Serializer is capable to handling them.
      kryo.register(classOf[scala.collection.immutable.HashSet.HashTrieSet[_]], new ScalaImmutableSetSerializer, 103)
      
      kryo.register(classOf[scala.collection.immutable.HashMap.HashMap1[_, _]], new ScalaImmutableMapSerializer, 104)
      kryo.register(classOf[scala.collection.immutable.HashMap.HashTrieMap[_, _]], new ScalaImmutableMapSerializer, 105)
    }
  }
  
  /**
   * Register the messages from the Ecology into this Kryo.
   */
  def registerMsgs(kryo:Kryo):Unit = {
    _msgDecls.foreach { decls => 
      QLog.spew(s"... registering message declarations...")
      decls.foreach { case (clazz, id) =>
        QLog.spew(s"    ${clazz.getCanonicalName} = $id")
        kryo.register(clazz, new serializers.TaggedFieldSerializer(kryo, clazz), id)
      }
    }    
  }
}
