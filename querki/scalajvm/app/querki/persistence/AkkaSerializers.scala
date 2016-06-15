package querki.persistence

import akka.actor.{ActorPath, ChildActorPath, ExtendedActorSystem}
import akka.serialization.Serialization

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

/**
 * Can we generalize this to ActorPath, and just register ChildActorPath to it?
 */
class ChildActorPathSerializer(val system:ExtendedActorSystem) extends Serializer[ChildActorPath] {
  override def read(kryo:Kryo, input:Input, typ:Class[ChildActorPath]):ChildActorPath = {
    val path = input.readString()
    // Bleah -- this is ugly, although probably works correctly:
    ActorPath.fromString(path).asInstanceOf[ChildActorPath]
  }
  
  override def write(kryo:Kryo, output:Output, obj:ChildActorPath) = {
    val defaultAddress = system.provider.getDefaultAddress
    output.writeString(obj.toSerializationFormatWithAddress(defaultAddress))
  }
}
