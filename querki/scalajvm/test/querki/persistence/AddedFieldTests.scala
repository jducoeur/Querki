package querki.persistence

import java.io._
import org.objenesis.strategy.StdInstantiatorStrategy
import com.esotericsoftware.kryo._
import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.util._
import com.romix.akka.serialization.kryo._
import com.romix.scala.serialization.kryo._

import querki.test._

/**
 * The tests for AddedField are similar to PersistenceTests, but separate because we have to actually
 * perform schema evolution in the middle of the test. So it requires setting up multiple instances of
 * Kryo.
 */
class AddedFieldTests extends QuerkiTests {
  
  def createKryoWith(classes:(Class[_], Int)*):Kryo = {
      // This code is intentionally redundant with the romix KryoSerializer, since we need to do this
      // outside the singleton Akka Serializer:
      val resolver = new SubclassResolver()
      val kryo = new ScalaKryo(resolver, new ListReferenceResolver(), new DefaultStreamFactory())
      val instStrategy = kryo.getInstantiatorStrategy.asInstanceOf[Kryo.DefaultInstantiatorStrategy]
      instStrategy.setFallbackInstantiatorStrategy(new StdInstantiatorStrategy())
      kryo.setInstantiatorStrategy(instStrategy)      
      kryo.setRegistrationRequired(true)
      kryo.register(classOf[AddedField[_]], new AddedFieldSerializer(), 10001)
      for (pair <- classes) 
        pair match {
          case (clz, id) => kryo.register(clz, new serializers.TaggedFieldSerializer(kryo, clz), id)
        }
      resolver.enable()   
      kryo
  }
  
  def write(kryo:Kryo, v:Any):Array[Byte] = {
    val outputStream = new ByteArrayOutputStream()
    val output = new Output(outputStream)
    kryo.writeClassAndObject(output, v)
    output.flush()
    outputStream.close()
    outputStream.toByteArray()    
  }
  
  def read(kryo:Kryo, bytes:Array[Byte]):Object = {
    val inStream = new ByteArrayInputStream(bytes)
    val input = new Input(inStream)
    kryo.readClassAndObject(input)
  }
  
  // First, make sure the basic framework is functioning:
  "Round-tripping through Kryo" should {
    "work" in {
      case class Bar(@KryoTag(1) baz:String)
      
      case class Floob(
        @KryoTag(1) foo:Int,
        @KryoTag(2) addedBar:Bar
      ) extends UseKryo
      
      val kryo = createKryoWith(
        (classOf[Bar], 101),
        (classOf[Floob], 102)
      )
      
      val bytes = write(kryo, Floob(42, Bar("hi")))      
      read(kryo, bytes) match {
        case Floob(foo, bar) => {
          assert(foo == 42)
          bar match {
            case Bar(baz) => assert(baz == "hi")
            case _ => fail("Didn't get a Bar!")
          }
        }
        case _ => fail("Didn't get a Floob!")
      }      
    }
  }
  
  "AddedField" should {
    // Check that we can write and read as expected:
    "work in the ordinary case" in {
      case class Bar(@KryoTag(1) baz:String)
      
      case class Floob(
        @KryoTag(1) foo:Int,
        @KryoTag(2) addedBar:AddedField[Bar]
      ) extends UseKryo
      
      val kryo = createKryoWith(
        (classOf[Bar], 101),
        (classOf[Floob], 102)
      )
      
      val bytes = write(kryo, Floob(42, Bar("hi")))      
      read(kryo, bytes) match {
        case Floob(foo, bar) => {
          assert(foo == 42)
          assert(bar.isDefined)
          assert(bar.toOption == Some(Bar("hi")))
          assert(bar.getOrElse(Bar("")) == Bar("hi"))
          assert(bar.map(_.baz) == Some("hi"))
        }
        case _ => fail("Didn't get a Floob!")
      }
    }
    
    // This is the complex test -- when a field has been added since we serialized.
    // This is really the point of the whole thing:
    "work when the AddedField is missing" in {
      case class Floob1(
        @KryoTag(1) foo:Int
      ) extends UseKryo
      
      val kryo1 = createKryoWith(
        (classOf[Floob1], 101)
      )
      
      val bytes = write(kryo1, Floob1(42))
      
      case class Bar(@KryoTag(1) baz:String)      
      case class Floob2(
        @KryoTag(1) foo:Int,
        @KryoTag(2) addedBar:AddedField[Bar]
      ) extends UseKryo
      
      val kryo2 = createKryoWith(
        (classOf[Floob2], 101),
        (classOf[Bar], 102)
      )
      
      read(kryo2, bytes) match {
        case Floob2(foo, bar) => {
          assert(foo == 42)
          assert(bar.isEmpty)
          assert(bar.toOption == None)
          assert(bar.getOrElse(Bar("")) == Bar(""))
          assert(bar.map(_.baz) == None)
        }
        case _ => fail("Didn't get a Floob!")
      }
    }
  }
}
