package querki.text

import querki.globals._
import querki.test._

/**
 * @author jducoeur
 */
class TextTests extends QuerkiTests {
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
