package querki.ql

import querki.test._

class QLTests extends QuerkiTests {
  // === _code ===
  "_code" should {
    def codeFormatted(result:String) = s"<pre>$result</pre>"
    
    "work with a Property and a received Thing" in {
      class TSpace extends CommonSpace {
        val textProp = new TestProperty(TextType, ExactlyOne, "My Text Prop")
        val myThing = new SimpleTestThing("My Thing", textProp("This is some text"))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> _code(My Text Prop)]]""") should
        equal (codeFormatted("This is some text"))
    }
  }
  
  "named parameters" should {
    "work as expected" in {
      implicit val s = commonSpace
      
      pql("""[[""FooBar"" -> _substring(end=4, start=1)]]""") should
        equal("ooB")
    }
  }
}