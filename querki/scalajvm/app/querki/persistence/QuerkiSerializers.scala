package querki.persistence

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

import models.{OID, ThingId}

/**
 * Optimized Serializer for OIDs.
 */
class OIDSerializer extends Serializer[OID] {
  override def read(kryo:Kryo, input:Input, typ:Class[OID]):OID = {
    OID(input.readLong())
  }
  
  override def write(kryo:Kryo, output:Output, obj:OID) = {
    output.writeLong(obj.raw)
  }
}

class ThingIdSerializer extends Serializer[ThingId] {
  override def read(kryo:Kryo, input:Input, typ:Class[ThingId]):ThingId = {
    ThingId(input.readString())
  }
  
  override def write(kryo:Kryo, output:Output, obj:ThingId) = {
    output.writeString(obj.toString)
  }  
}
