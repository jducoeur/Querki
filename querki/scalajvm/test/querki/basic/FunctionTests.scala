package querki.basic

import querki.test._

class FunctionTests extends QuerkiTests {
  lazy val QLType = Basic.QLType
  
  "A Function" should {
    "be able to resolve using lexical context" in {
      // Note that this has outerText invoking myTextProp *lexically*, not via the received context
      class TSpace extends CommonSpace {
        val myFunc = new TestProperty(QLType, ExactlyOne, "My Function")
        val outerText = new TestProperty(TextType, ExactlyOne, "Outer Text")
        val myNumber = new TestProperty(Core.IntType, ExactlyOne, "My Number")
        val otherNumber = new TestProperty(Core.IntType, ExactlyOne, "Other Number")
        
        val callingThing = 
          new SimpleTestThing("Caller", 
              myFunc("My Number"),
              outerText("From the outside: [[Other -> My Function]]"))
        val otherThing = new SimpleTestThing("Other", myNumber(42), otherNumber(13))
      }
      implicit val s = new TSpace
      
      pql("""[[Caller -> Outer Text]]""") should equal ("From the outside: 42")      
    }
    
    "be able to resolve using an instance's lexical context" in {
      // Note that this has outerText invoking myTextProp *lexically*, not via the received context
      class TSpace extends CommonSpace {
        val myFunc = new TestProperty(QLType, ExactlyOne, "My Function")
        val outerText = new TestProperty(TextType, ExactlyOne, "Outer Text")
        val myNumber = new TestProperty(Core.IntType, ExactlyOne, "My Number")
        val otherNumber = new TestProperty(Core.IntType, ExactlyOne, "Other Number")
        
        val callingModel = new SimpleTestThing("Calling Model", myFunc("My Number"))
        val callingThing = 
          new TestThing("Caller", callingModel,
              outerText("From the outside: [[Other -> My Function]]"))
        val otherThing = new SimpleTestThing("Other", myNumber(42), otherNumber(13))
      }
      implicit val s = new TSpace
      
      pql("""[[Caller -> Outer Text]]""") should equal ("From the outside: 42")      
    }
    
    "resolve using the calling context if applicable" in {
      // Since Other has My Function defined on it, we use that by preference:
      class TSpace extends CommonSpace {
        val myFunc = new TestProperty(QLType, ExactlyOne, "My Function")
        val outerText = new TestProperty(TextType, ExactlyOne, "Outer Text")
        val myNumber = new TestProperty(Core.IntType, ExactlyOne, "My Number")
        val otherNumber = new TestProperty(Core.IntType, ExactlyOne, "Other Number")
        
        val callingThing = 
          new SimpleTestThing("Caller", 
              myFunc("My Number"),
              outerText("From the outside: [[Other -> My Function]]"))
        val otherThing = new SimpleTestThing("Other", myNumber(42), myFunc("Other Number"), otherNumber(13))
      }
      implicit val s = new TSpace
      
      pql("""[[Caller -> Outer Text]]""") should equal ("From the outside: 13")      
    }
    
    "resolve using the defining context if there is one" in {
      // Since Other has My Function defined on it, we use that by preference:
      class TSpace extends CommonSpace {
        val myFunc = new TestProperty(QLType, ExactlyOne, "My Function")
        val outerText = new TestProperty(TextType, ExactlyOne, "Outer Text")
        val myNumber = new TestProperty(Core.IntType, ExactlyOne, "My Number")
        val otherNumber = new TestProperty(Core.IntType, ExactlyOne, "Other Number")
        
        val callingThing = 
          new SimpleTestThing("Caller", 
              myFunc("My Number"),
              outerText("From the outside: [[Other -> External Thing.My Function]]"))
        val otherThing = new SimpleTestThing("Other", myNumber(42), myFunc("Other Number"), otherNumber(13))
        val externalThing = new SimpleTestThing("External Thing", myFunc("Name"))
      }
      implicit val s = new TSpace
      
      pql("""[[Caller -> Outer Text]]""") should equal ("From the outside: Other")      
    }
  }
}