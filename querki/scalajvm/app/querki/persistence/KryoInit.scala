package querki.persistence

import akka.actor.{ChildActorPath, ExtendedActorSystem}

import com.esotericsoftware.kryo._

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
    KryoInit._actorSystem.map { actorSystem =>
      akka.actor.AkkaHack.setupPrivateSerializers(kryo, actorSystem)
      kryo.register(classOf[ChildActorPath], new ChildActorPathSerializer(actorSystem), 100)
    }
    
    // Then, register the actual application messages, which have been declared in 
    // their Ecots, so their ids reflect the Ecot they come from:
    KryoInit._msgDecls.map { decls =>
      QLog.spew(s"... registering message declarations...")
      decls.foreach { case (clazz, id) =>
        QLog.spew(s"    ${clazz.getCanonicalName} = $id")
        kryo.register(clazz, new serializers.TaggedFieldSerializer(kryo, clazz), id)
      }
    }
    QLog.spew(s"... done customizing.")
    
    // TODO: once Kryo 3.0.4 is released, figure out how to leverage
    // TaggedFieldSerializer.setIgnoreUnknownTags(true) -- this should give
    // us rudimentary forward compatibility. That probably doesn't matter
    // much for persistence, but will be useful if we ever use Kryo for network
    // serialization in a rolling-upgrades scenario.
  }
}

/**
 * Static kludges to make Kryo initialization work.
 */
object KryoInit {
  var _msgDecls:Option[Seq[(Class[_], Int)]] = None
  var _actorSystem:Option[ExtendedActorSystem] = None
}
