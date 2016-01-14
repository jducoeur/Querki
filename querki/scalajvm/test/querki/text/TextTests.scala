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
