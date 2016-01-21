package querki.text

import querki.globals._
import querki.test._

/**
 * @author jducoeur
 */
class TextTests extends QuerkiTests {
  "_matchCase" should {
    "work in simple cases" in {
      class TSpace extends CommonSpace {
        val matchFunc = new TestProperty(Basic.QLType, ExactlyOne, "Matcher")
        val textThing = new SimpleTestThing("Text Thing", optTextProp("something"),
            matchFunc("My Optional Text -> _matchCase"))
      }
      implicit val s = new TSpace
      
      pql("""[[Text Thing -> Matcher]]""") should equal("Something")
      pql("""[[Text Thing -> matcher]]""") should equal("something")
    }
    
    "work with one explicit level" in {
      class TSpace extends CommonSpace {
        val matchFunc = new TestProperty(Basic.QLType, ExactlyOne, "Matcher")
        val textThing = new SimpleTestThing("Text Thing", optTextProp("something"),
            matchFunc("My Optional Text -> _matchCase(1)"))
      }
      implicit val s = new TSpace
      
      pql("""[[Text Thing -> Matcher]]""") should equal("Something")
      pql("""[[Text Thing -> matcher]]""") should equal("something")
    }
    
    "look up the stack a level" in {
      class TSpace extends CommonSpace {
        val matchFunc = new TestProperty(Basic.QLType, ExactlyOne, "Matcher")
        val caller = new TestProperty(Basic.QLType, ExactlyOne, "Caller")
        val textThing = new SimpleTestThing("Text Thing", optTextProp("something"),
            caller("Matcher"),
            matchFunc("My Optional Text -> _matchCase(2)"))
      }
      implicit val s = new TSpace
      
      pql("""[[Text Thing -> Caller]]""") should equal("Something")
      pql("""[[Text Thing -> caller]]""") should equal("something")
    }
    
    // This is the motivating use case, from the LARP App:
    "go two levels including the defining context" in {
      class TSpace extends CommonSpace {
        val nominative = new TestProperty(TextType, ExactlyOne, "Nominative")
        val gender = new SimpleTestThing("Gender")
        val male = new TestThing("Male", gender, nominative("he"))
        
        val charGender = new TestProperty(LinkType, ExactlyOne, "Character Gender")
        val pronoun = new TestProperty(Basic.QLType, ExactlyOne, "Pronoun")
        val ze = new TestProperty(Basic.QLType, ExactlyOne, "Ze")
        val character = 
          new SimpleTestThing("Character",
              charGender(),
              pronoun("Character Gender -> $_defining -> _matchCase(2)"),
              ze("Nominative.Pronoun"))
        val joe = new TestThing("Joe", character, charGender(male))
      }
      implicit val s = new TSpace
      
      pql("""[[Joe -> Ze]]""") should equal("He")
      pql("""[[Joe -> ze]]""") should equal("he")
    }
  }
  
  "_substring" should {
    "allow oid manipulation" in {
      implicit val s = commonSpace
      
      pql("[[Sandbox -> _oid -> _substring(1)]]") should
        // OID.toString already lacks the dot at the front:
        equal(s.sandbox.id.toString())
    }
    
    "work with one parameter" in {
      implicit val s = commonSpace
      
      pql("""[[""FooBar"" -> _substring(3)]]""") should
        equal("Bar")
    }
    
    "work with two parameters" in {
      implicit val s = commonSpace
      
      pql("""[[""FooBar"" -> _substring(1, 4)]]""") should
        equal("ooB")
    }
    
    "cope with parameters that are out of bounds" in {
      implicit val s = commonSpace
      
      pql("""[[""FooBar"" -> _substring(1, 7)]]""") should
        equal("ooBar")
      pql("""[[""FooBar"" -> _substring(7, 10)]]""") should
        equal("")
    }
  }
}
