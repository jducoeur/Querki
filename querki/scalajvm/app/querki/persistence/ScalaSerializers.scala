package querki.persistence

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

import querki.time.DateTime

/**
 * The trivial serializer for None. Since Option consists of only None and Some, and they
 * serialize differently, we don't bother with SubclassSerializer for them.
 */
class NoneSerializer extends Serializer[None.type] {
  override def read(kryo:Kryo, input:Input, typ:Class[None.type]):None.type = None
  
  override def write(kryo:Kryo, output:Output, obj:None.type) = {}
}

class SomeSerializer extends Serializer[Some[_]] {
  override def read(kryo:Kryo, input:Input, typ:Class[Some[_]]):Some[_] = {
    Some(kryo.readClassAndObject(input))
  }
  
  override def write(kryo:Kryo, output:Output, obj:Some[_]) = {
    kryo.writeClassAndObject(output, obj.get)
  }  
}

class DateTimeSerializer extends Serializer[DateTime] {
  override def read(kryo:Kryo, input:Input, typ:Class[DateTime]):DateTime = {
    new DateTime(input.readLong())
  }
  
  override def write(kryo:Kryo, output:Output, obj:DateTime) = {
    output.writeLong(obj.getMillis)
  }
}
