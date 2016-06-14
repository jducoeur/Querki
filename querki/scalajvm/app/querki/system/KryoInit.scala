package querki.system

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
    kryo.setDefaultSerializer(classOf[serializers.TaggedFieldSerializer[_]])
  }
}
