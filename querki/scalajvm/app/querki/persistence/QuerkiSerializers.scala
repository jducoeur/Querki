package querki.persistence

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

import models.OID

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
