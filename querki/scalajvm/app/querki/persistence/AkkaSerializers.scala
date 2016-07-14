package querki.persistence

import akka.actor.{ActorPath, ExtendedActorSystem}
import akka.serialization.Serialization

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

class ActorPathSerializer(val system:ExtendedActorSystem) extends Serializer[ActorPath] {
  override def read(kryo:Kryo, input:Input, typ:Class[ActorPath]):ActorPath = {
    val path = input.readString()
    // Bleah -- this is ugly, although probably works correctly:
    ActorPath.fromString(path).asInstanceOf[ActorPath]
  }
  
  override def write(kryo:Kryo, output:Output, obj:ActorPath) = {
    val defaultAddress = system.provider.getDefaultAddress
    output.writeString(obj.toSerializationFormatWithAddress(defaultAddress))
  }
}
