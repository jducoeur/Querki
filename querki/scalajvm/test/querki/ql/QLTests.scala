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
    // This test is cognate to the way $_defining is used for Pronouns in the LARP App:
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
  
  "_self" should {
    "work normally" in {
      implicit val s = commonSpace
      
      // Silly test, but we're proving that My Optional Text does no dereference here:
      pql("""[[My Instance -> My Optional Text._self]]""") should
        equal(linkText(s.optTextProp))
    }
    
    "throw a decent error if misused" in {
      implicit val s = commonSpace
      
      pql("""[[My Instance -> My Optional Text -> _self]]""") should
        equal(expectedWarning("QL.self.notDotted"))
    }
  }
  
  "bound names" should {
    class TSpace extends CommonSpace {
      val myInt = new TestProperty(Core.IntType, ExactlyOne, "My Int")
      
      val toy = new SimpleTestThing("Toy", myInt())
      val rock = new TestThing("Rock", toy, myInt(1))
      val paper = new TestThing("Paper", toy, myInt(2))
      val scissors = new TestThing("Scissors", toy, myInt(3))
    }
    "work inside _foreach" in {
      implicit val s = new TSpace
      
      pql("""[[Toy._instances -> _foreach(+$toy ->
          Link Name -> +$name ->
          $toy -> My Int -> +$int ->
          ""[[$name]]: [[$int]]"")]]""") should
        equal ("\nPaper: 2\nRock: 1\nScissors: 3")
    }
  }
}