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
  
  "$_defining" should {
    "work from another local Function" in {
      class TSpace extends CommonSpace {
        val dropTwo = new TestProperty(Basic.QLType, ExactlyOne, "Drop Two")
        val dropped = new TestProperty(Basic.QLType, ExactlyOne, "Dropped List")
        val textProp = new TestProperty(TextType, QList, "List of Text")
        val withList = new SimpleTestThing("With List", textProp("One", "Two", "Three", "OClock Rock"),
            dropped("List of Text.Drop Two"),
            dropTwo("$_defining -> _drop(2)"))
      }
      implicit val s = new TSpace
      
      pql("""[[With List -> Dropped List -> _commas]]""") should
        equal("Three, OClock Rock")
    }
  }
  
  "named parameters" should {
    "work without spaces" in {
      implicit val s = commonSpace
      
      pql("""[[""FooBar"" -> _substring(end=4, start=1)]]""") should
        equal("ooB")
    }
    
    "work with spaces" in {
      implicit val s = commonSpace
      
      pql("""[[""FooBar"" -> _substring(end = 4, start = 1)]]""") should
        equal("ooB")
    }  
  }
}