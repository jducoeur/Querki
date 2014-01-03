package querki.ecology

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import querki.values.SpaceState

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
  
  trait TestInterface3 extends EcologyInterface
  
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
    
    "successfully register and initialize a couple of interfaces without dependencies" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends Module(eco) with TestInterface1
      class Ecot2(val moduleId:Short) extends Module(eco) with TestInterface2
      
      val ecot1 = new Ecot1(1)
      val ecot2 = new Ecot2(2)
      
      assert(eco.manager.isRegistered[TestInterface1])
      assert(eco.manager.isRegistered[TestInterface2])
      assert(!eco.manager.isRegistered[TestInterface3])
      
      val finalState = eco.manager.init(models.system.SystemSpace.initialSystemState)
    }
  }
}
