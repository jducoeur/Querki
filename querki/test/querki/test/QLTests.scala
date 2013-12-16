package querki.test

import models.system._

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

class QLTests extends QuerkiTests {
  // === Attachments ===
  "Photos" should {
    "self-render when linked" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Photo]]""") should 
        equal ("""![My Photo](a/My-Photo)""")
    }
  }
  
  "Arrows" should {
    "work inline" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances -> _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))              
    }
    
    "work at end of line" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances -> 
          | _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))              
    }
    
    "work at start of line" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances 
          |-> _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))              
    }
  }
  
  "Ordinary references" should {
    "return through the initial context" in {
      processQText(commonThingAsContext(_.withUrl), "[[My Optional URL]]") should
        equal ("""[http://www.google.com/](http://www.google.com/)""")
    }
    
    "return through an interrupted context" in {
      processQText(commonThingAsContext(_.sandbox), "[[With URL -> My Optional URL]]") should
        equal ("""[http://www.google.com/](http://www.google.com/)""")      
    }
  }
  
  "Display names" should {
    "work with backticks" in {
      processQText(commonThingAsContext(_.sandbox), """[[`My name is "interesting"!`]]""") should
        equal ("""[My name is &quot;interesting&quot;!](Interesting-Display-Name)""")
    }
    
    "work normally if the name is simple" in {
      class TSpace extends CommonSpace {
        val nameThing = new SimpleTestThing("NameProp name", DisplayNameProp("My Display Name"))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.nameThing)), "[[My Display Name]]") should
        equal ("""[My Display Name](NameProp-name)""")      
    }
  }
  
  "$context references" should {
    "work trivially" in {
      class TSpace extends CommonSpace {
        val myMethod = new TestProperty(QLType, ExactlyOne, "My Method")
        
        val thingWithMethod = new SimpleTestThing("Methodical", myMethod("""$_context -> Name"""))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[My Method]]") should
        equal ("Methodical")
    }
    
    "work in a parameter" in {
      class TSpace extends CommonSpace {
        val myMethod = new TestProperty(QLType, ExactlyOne, "My Method")
        
        val thingWithMethod = new SimpleTestThing("Methodical", myMethod("""_section($_context)"""))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[My Method]]") should
        equal ("[Methodical](Methodical)")
    }
    
    "work when used as a complex parameter" in {
      class TSpace extends CommonSpace {
        val myQLProp = new TestProperty(QLType, ExactlyOne, "My Method")
        
        val myEnum = new SimpleTestThing("My Enum")
        val enum1 = new TestThing("Enum 1", myEnum)
        val enum2 = new TestThing("Enum 2", myEnum)
        
        val myEnumProp = new TestProperty(LinkType, ExactlyOne, "Enum Prop", LinkModelProp(myEnum))
        
        val thingWithMethod = new SimpleTestThing("Methodical", myQLProp("""_if(_equals($_context, With Enum 1 -> Enum Prop), ""You gave me Enum 1!"", ""Wrong thing! Humph!"")""".stripMargin))
        val withEnum1 = new SimpleTestThing("With Enum 1", myEnumProp(enum1))
        val withEnum2 = new SimpleTestThing("With Enum 2", myEnumProp(enum2))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[Enum 1 -> Methodical.My Method]]") should
        equal ("You gave me Enum 1!")
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[Enum 2 -> Methodical.My Method]]") should
        equal ("Wrong thing! Humph!")
    }
  }
  
  "Parameter references" should {
    "work trivially" in {
      class TSpace extends CommonSpace {
        val myMethod = new TestProperty(QLType, ExactlyOne, "My Method")
        
        val thingWithMethod = new SimpleTestThing("Methodical", myMethod("""$_1 -> Name"""))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[My Method(Methodical)]]") should
        equal ("Methodical")
    }
    
    "work when used as a complex parameter" in {
      class TSpace extends CommonSpace {
        val myQLProp = new TestProperty(QLType, ExactlyOne, "My Method")
        
        val myEnum = new SimpleTestThing("My Enum")
        val enum1 = new TestThing("Enum 1", myEnum)
        val enum2 = new TestThing("Enum 2", myEnum)
        
        val myEnumProp = new TestProperty(LinkType, ExactlyOne, "Enum Prop", LinkModelProp(myEnum))
        
        val thingWithMethod = new SimpleTestThing("Methodical", myQLProp("""_if(_equals($_1, With Enum 1 -> Enum Prop), ""You gave me Enum 1!"", ""Wrong thing! Humph!"")""".stripMargin))
        val withEnum1 = new SimpleTestThing("With Enum 1", myEnumProp(enum1))
        val withEnum2 = new SimpleTestThing("With Enum 2", myEnumProp(enum2))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[Methodical.My Method(Enum 1)]]") should
        equal ("You gave me Enum 1!")
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[Methodical.My Method(Enum 2)]]") should
        equal ("Wrong thing! Humph!")      
    }
    
    "work with multiple parameters" in {
      class TSpace extends CommonSpace {
        val myQLProp = new TestProperty(QLType, ExactlyOne, "My Method")
        
        val myEnum = new SimpleTestThing("My Enum")
        val enum1 = new TestThing("Enum 1", myEnum)
        val enum2 = new TestThing("Enum 2", myEnum)
        
        val myEnumProp = new TestProperty(LinkType, ExactlyOne, "Enum Prop", LinkModelProp(myEnum))
        
        val thingWithMethod = new SimpleTestThing("Methodical", myQLProp("""_if(_equals($_1, $_2 -> Enum Prop), ""Yep, that has the right value"", ""Wrong value!"")""".stripMargin))
        val withEnum1 = new SimpleTestThing("With Enum 1", myEnumProp(enum1))
        val withEnum2 = new SimpleTestThing("With Enum 2", myEnumProp(enum2))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[Methodical.My Method(Enum 1, With Enum 1)]]") should
        equal ("Yep, that has the right value")
      processQText(thingAsContext[TSpace](space, (_.thingWithMethod)), "[[Methodical.My Method(Enum 1, With Enum 2)]]") should
        equal ("Wrong value!")            
    }
  }
  
  "Comments" should {
    "work as the whole body" in {
      processQText(commonThingAsContext(_.sandbox), "[[// This is a comment, which does nothing]]") should
        equal ("")
    }
    
    "work at the beginning of a line" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances ->
          |// This is a comment
          |  _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))        
    }
    
    "work indented" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances ->
          |  // This is a comment
          |  _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))        
    }
    
    "work after an arrow" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances -> // This is a comment
          |  _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))        
    }
    
    "work before an arrow" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances // This is a comment
          |  -> _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))
    }
    
    "work at the beginning of the expression" in {
      processQText(commonThingAsContext(_.sandbox), """[[// This is a comment
          |My Model._instances ->
          |  _sort]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))        
    }
    
    "work at the end of the expression" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances ->
          |  _sort
          |// This is a comment]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))
    }
    
    "work on same line as the end of the expression" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances ->
          |  _sort // This is a comment]]""".stripMargin) should equal (listOfLinkText(commonSpace.instance))        
    }  
  }
}