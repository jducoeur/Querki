package querki.persistence

import java.io._

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io._

import org.objenesis.strategy.StdInstantiatorStrategy

import querki.globals._

/**
 * Mix-in trait for tests of persistent classes. The main thing here is the
 * serialRoundtrip() function, which serializes the given object and then deserializes
 * and returns it, so that you can check that that went smoothly.
 * 
 * It is *strongly* recommended that *all* persistent classes get a unit test of
 * this sort. What it's really testing is that the necessary bits and bobs are
 * getting registered in the Ecology.
 */
class PersistTest(env:PersistEnv) {
  /**
   * This creates a Kryo (based on the registrations in the Ecology), writes out the given object,
   * reads it back in again and returns it. You will usually want to follow this with assertions that
   * the read object matches the written one.
   */
  def serialRoundtrip[T](in:T):T = {
    // First, sanity-check that the type is marked as UseKryo:
    env.checkObj(in)
    
    val kryo = new Kryo()
    // In the running system, this gets set from config by the romix library, but we're using raw Kryo:
    kryo.setRegistrationRequired(true)
    new KryoInit().customize(kryo)
    // From the romix code, necessary to cope with case classes:
    kryo.setInstantiatorStrategy(new StdInstantiatorStrategy())
    val outStream = new ByteArrayOutputStream()
    val output = new Output(outStream)   
    kryo.writeClassAndObject(output, in)
    output.flush()
    val inStream = new ByteArrayInputStream(outStream.toByteArray())
    val input = new Input(inStream)
    kryo.readClassAndObject(input).asInstanceOf[T]
  }
  
  /**
   * Convenience function over serialRoundtrip. You can use this iff the class you're testing has a
   * good == function. (That is, if it's a case class, which it usually is.)
   */
  def checkSerialization[T](builder: => T) = {
    val orig = builder
    val copy = serialRoundtrip(orig)
    QLog.spew(s"Checking equality of $copy")
    env.checkEquality(copy, orig)
  }
}
