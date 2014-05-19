package querki.test

import querki.ecology._

import querki.util.PublicException

class PropertyTests extends QuerkiTests {
  "Display Name Property" should {
    "require a length of at least 1" in {
      DisplayNameProp.validate("hello!", commonState)
      DisplayNameProp.validate("a", commonState)
      
      val ex = intercept[PublicException] {
        DisplayNameProp.validate("", commonState)
      }
      assert(ex.msgName == "Types.Text.tooShort")
      
      val ex2 = intercept[PublicException] {
        DisplayNameProp.validate(" \t\n", commonState)        
      }
      assert(ex.msgName == "Types.Text.tooShort")
    }
  }
    
  "Property references" should {
    "dereference a List of Things correctly" in {
      class TSpace extends CommonSpace {
        val myProp = new TestProperty(Core.IntType, Optional, "Number Prop")
        
        val myModel = new SimpleTestThing("Test Model", myProp(0))
        
        val instance1 = new TestThing("Instance 1", myModel, myProp(1))
        val instance2 = new TestThing("Instance 2", myModel, myProp(2))
        val instance3 = new TestThing("Instance 3", myModel, myProp(3))
        val instance4 = new TestThing("Instance 4", myModel, myProp(4))
        val instance5 = new TestThing("Instance 5", myModel, myProp(5))
      }
      implicit val s = new TSpace
      
      pql("""[[Test Model._instances -> Number Prop -> _sort]]""") should
        equal("""
            |1
            |2
            |3
            |4
            |5""".stripReturns)
    }
    
    "use the lexical context in text" in {
      // Note that this has outerText invoking myTextProp *lexically*, not via the received context
      class TSpace extends CommonSpace {
        val myTextProp = new TestProperty(TextType, ExactlyOne, "My Text Prop")
        val outerText = new TestProperty(TextType, ExactlyOne, "Outer Text")
        val myNumber = new TestProperty(Core.IntType, ExactlyOne, "My Number")
        
        val callingThing = 
          new SimpleTestThing("Caller", 
              myTextProp("The number is [[My Number]]"),
              outerText("From the outside: [[Other -> My Text Prop]]"))
        val otherThing = new SimpleTestThing("Other", myNumber(42))
      }
      implicit val s = new TSpace
      
      pql("""[[Caller -> Outer Text]]""") should equal ("From the outside: The number is 42")
    }
  }
}