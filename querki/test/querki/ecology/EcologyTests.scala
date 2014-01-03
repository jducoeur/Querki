package querki.ecology

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import querki.test._

import modules._

/**
 * TODO: this should probably go back to inheriting from QuerkiTests, once those are working again.
 */
class EcologyTests extends WordSpec
  with ShouldMatchers
  with BeforeAndAfterAll
{
  trait TestInterface1 extends EcologyInterface {
    
  }
  
  trait TestInterface2 extends EcologyInterface {
    
  }
  
  "The Ecology" should {
    "throw an exception if I double-register an interface" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends Module(eco) with TestInterface1
      class Ecot2(val moduleId:Short) extends Module(eco) with TestInterface1
      
      val ecot1 = new Ecot1(1)
      intercept[Exception] {
        val ecot2 = new Ecot2(2)
      }
    }
    
    "successfully register a couple of interfaces" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends Module(eco) with TestInterface1
      class Ecot2(val moduleId:Short) extends Module(eco) with TestInterface2
      
      val ecot1 = new Ecot1(1)
      val ecot2 = new Ecot2(2)
      
      val registered = eco.manager.allRegisteredInterfaces
      assert(registered.contains(classOf[TestInterface1]))
      assert(registered.contains(classOf[TestInterface2]))
      
      println("Registered Interfaces:")
      eco.manager.allRegisteredInterfaces.foreach(interface => println(s"    ${interface.getSimpleName}"))
    }
  }
}
