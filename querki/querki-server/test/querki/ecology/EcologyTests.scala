package querki.ecology

import org.scalatest.{WordSpec, BeforeAndAfterAll, Matchers}

import querki.values.SpaceState

import querki.test._

class EcologyTests extends WordSpec
  with Matchers
  with BeforeAndAfterAll
{
  trait TestInterface1 extends EcologyInterface {
    
  }
  
  trait TestInterface2 extends EcologyInterface {
    def getTheAnswer:Int = 1
  }
  
  trait TestInterface3 extends EcologyInterface
  
  def doInit(eco:Ecology):SpaceState = {
    // The Ecology itself assumes that Core is registered, and System is needed to finish things up:
    new querki.core.CoreModule(eco)
    new querki.system.SystemEcot(eco)
    // HACK: since init() differs depending on implementation, we need to cast to that:
    eco.manager.asInstanceOf[EcologyImpl].init(querki.system.InitialSystemState.create(eco), { (props, name) => None })
  }
  
  "The Ecology" should {
    "throw an exception if I double-register an interface" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1
      class Ecot2(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1
      
      val ecot1 = new Ecot1(1)
      intercept[AlreadyRegisteredInterfaceException[_,_]] {
        val ecot2 = new Ecot2(2)
      }
    }
    
    "successfully register and initialize a couple of interfaces without dependencies" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1
      class Ecot2(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface2
      
      val ecot1 = new Ecot1(1)
      val ecot2 = new Ecot2(2)
      
      intercept[UninitializedInterfaceException] {
        eco.api[TestInterface1]
      }
      
      assert(eco.manager.isRegistered[TestInterface1])
      assert(eco.manager.isRegistered[TestInterface2])
      assert(!eco.manager.isRegistered[TestInterface3])
      
      val finalState = doInit(eco)
      
      val interface1 = eco.api[TestInterface1]
      intercept[UnknownInterfaceException] {
        eco.api[TestInterface3]
      }
    }
    
    "successfully register, initialize and terminate Ecots with dependencies" in {
      val eco = new EcologyImpl
      
      // The order of termination is deterministic, due to the dependencies. So check
      // that it came out right:
      var termOrder:Seq[Ecot] = Seq.empty
      
      class Ecot1(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1 {
        val interface2 = initRequires[TestInterface2]
        
        lazy val answer:Int = interface2.getTheAnswer
        
        override def term() = {
          termOrder = termOrder :+ this
        }
      }
      class Ecot2(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface2 {
        override val getTheAnswer = 42
        
        override def term() = {
          termOrder = termOrder :+ this
        }
      }
      
      val ecot1 = new Ecot1(1)
      val ecot2 = new Ecot2(2)
      
      val finalState = doInit(eco)
      
      assert(ecot1.answer == 42)
      
      eco.manager.term
      
      assert(termOrder(0) == ecot1)
      assert(termOrder(1) == ecot2)
    }
    
    "throw if I try to use a dependency before initialization" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1 {
        val interface2 = initRequires[TestInterface2]
        
        lazy val answer:Int = interface2.getTheAnswer
        
        val causeError = answer
      }
      class Ecot2(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface2 {
        override val getTheAnswer = 42
      }
      
      val ecot2 = new Ecot2(2)
      intercept[UninitializedInterfaceException] {
        val ecot1 = new Ecot1(1)
      }
    }
    
    "allow me to use a dependency during initialization" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1 {
        val interface2 = initRequires[TestInterface2]
        
        lazy val answer:Int = interface2.getTheAnswer
        
        override def init() = {
          myAnswer = Some(answer)
        }
        var myAnswer:Option[Int] = None
      }
      class Ecot2(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface2 {
        override val getTheAnswer = 42
      }
      
      val ecot1 = new Ecot1(1)
      val ecot2 = new Ecot2(2)
      
      val finalState = doInit(eco)
      
      assert(ecot1.myAnswer == Some(42))
    }
    
    "detect a missing interface in initialization" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1 {
        val interface2 = initRequires[TestInterface2]
        
        lazy val answer:Int = interface2.getTheAnswer
        
        override def init() = {
          myAnswer = Some(answer)
        }
        var myAnswer:Option[Int] = None
      }
      class Ecot2(val moduleId:Short) extends QuerkiEcot(eco)
      
      val ecot1 = new Ecot1(1)
      val ecot2 = new Ecot2(2)

      intercept[InitMissingInterfaceException[_,_]] {
        val finalState = doInit(eco)
      }
    }
    
    "detect a dependency loop during initialization" in {
      val eco = new EcologyImpl
      class Ecot1(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface1 {
        val interface2 = initRequires[TestInterface2]
      }
      class Ecot2(val moduleId:Short) extends QuerkiEcot(eco) with TestInterface2 {
        val interface1 = initRequires[TestInterface1]
      }
      
      val ecot1 = new Ecot1(1)
      val ecot2 = new Ecot2(2)

      intercept[InitDependencyLoopException[_,_]] {
        val finalState = doInit(eco)
      }
    }    
  }
}
