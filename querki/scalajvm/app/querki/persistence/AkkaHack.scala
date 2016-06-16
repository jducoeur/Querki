package akka.actor

import com.esotericsoftware.kryo._
import com.esotericsoftware.kryo.io.{Input, Output}

import querki.globals._

/**
 * This exists to get around visibility rules, so we can set up serializers for internal Akka types.
 */
object AkkaHack {
  def setupPrivateSerializers(kryo:Kryo, actorSystem:ExtendedActorSystem) = {
    kryo.register(classOf[LocalActorRef], new com.romix.akka.serialization.kryo.ActorRefSerializer(actorSystem), 101)    
  }
}
