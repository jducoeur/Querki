package querki.system

import com.esotericsoftware.kryo._
import serializers.TaggedFieldSerializer

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

object KryoInit {
  var _msgDecls:Option[Seq[(Class[_], Int)]] = None
}