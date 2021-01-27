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
  
  // === $_defining ===
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
  
  // === named parameters ===
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
  
  "_isBound" should {
    "work with bound names" in {
      implicit val s = commonSpace
      
      pql("""[[_isBound($thingy)]]""") should equal ("false")
      pql("""[[""blah"" -> +$thingy; _isBound($thingy)]]""") should equal ("true")
    }
    
    "work with query parameters" in {
      implicit val s = commonSpace
      
      // A bit more involved than usual, since we have to fake a real request with query params:
      val metadata = querki.api.RequestMetadata("1.0", Map("foo" -> "12"))
      val rc = SimpleTestRequestContext(s.owner.mainIdentity.id, Some(metadata))
      val context = s.state.thisAsContext(rc, s.state, ecology)
      
      processQText(context, """[[$foo]]""") should equal ("12")
      processQText(context, """[[_isBound($bar)]]""") should equal ("false")
      processQText(context, """[[_isBound($foo)]]""") should equal ("true")
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
    
    "be passed from phrase to phrase within an expression" in {
      implicit val s = new TSpace
      
      pql("""[[""Hello"" -> +$greeting
            |  ""there!"" -> +$greeting2
            |  ""[[$greeting]] [[$greeting2]]""]]""".stripMargin) should
        equal("Hello there!")
    }
    
    "work inside of a QText block" in {
      implicit val s = new TSpace
      
      // This is a bit contrived, but proves the expectation:
      pql("""[[Toy._instances -> 
        |""[[
        |    Link Name -> +$name
        |    My Int -> +$int
        |    ""[[$name]]: [[$int]]""
        |]]""]]""".stripMargin) should
        equal ("\nPaper: 2\nRock: 1\nScissors: 3")
    }
    
    "show an error if you reassign a name inside an expression" in {
      implicit val s = new TSpace
      
      pql("""[[
        |""foo"" -> +$name
        |""bar"" -> +$name
        |""[[$name]]""]]""".stripMargin) should
        // TODO: This is ugly and hardcoded. We should have a better way: 
        equal (expectedWarning("Attempting to reassign $name -- you may only say +$name once"))
    }
    
    // This demonstrates the fix to a bug, that $_context was stomping the bindings before it:
    "work despite $_context" in {
      implicit val s = new TSpace
      
      // This is a bit contrived, but proves the expectation:
      pql("""[[Toy._instances -> 
        |""[[
        |    Link Name -> +$name
        |    My Int -> +$int
        |    $_context -> ""[[$name]]: [[$int]]""
        |]]""]]""".stripMargin) should
        equal ("\nPaper: 2\nRock: 1\nScissors: 3")
    }
  }
  
  "local functions" should {
    "work in a simple case" in {
      implicit val s = new CDSpace
      
      pql("""[[Album._instances -> _groupBy(Artists) -> +$byArtist
               _def $Show Artist = _first -> Link Name
               $byArtist -> _groupGet(Blackmores Night) -> $Show Artist
             ]]""".stripReturns) should equal ("Fires at Midnight")
    }
    
    "work with parameters" in {
      implicit val s = new CDSpace
      
      pql("""[[Album._instances -> _groupBy(Artists) -> +$byArtist
               _def $From First Album($artist, $field) = $byArtist -> _groupGet($artist) -> _first -> $field
               $From First Album(Blackmores Night, Link Name)
               $From First Album(Eurythmics, Artists -> Genres)
             ]]""".stripReturns) should equal ("Fires at Midnight\n[Rock](Rock)")
    }
  }
  
  "List Literals" should {
    "work with text" in {
      implicit val s = new CommonSpace
      
      pql("""[[<""hello"", "" there""> -> _join]]""") should
        equal("hello there")
    }
    
    "work with Things" in {
      implicit val s = new CommonSpace
      
      pql("""[[<Trivial, My Instance>]]""") should
        equal(listOfLinkText(s.trivialThing, s.instance))
    }
    
    "Work with an empty first Element" in {
      implicit val s = commonSpace
      
      pql("""[[<_if(false, 1), 2, 3>]]""") should equal ("\n2\n3")
    }
  }
  
  "Numeric Literals" should {
    "work normally" in {
      implicit val s = commonSpace
      
      pql("""[[5 -> _plus(7)]]""") should equal ("12")
      pql("""[[-5 -> _plus(7)]]""") should equal ("2")
      pql("""[[5 -> _plus(-2)]]""") should equal ("3")
    }
  }

  "Self-referential expressions" should {
    // Note that the failure mode for this one is an infinite loop!
    // This is a pretty simplistic check, but it guards against QI.bu6ofru
    "not destroy the world" in {
      class TSpace extends CommonSpace {
        val deadlyProp = new SimpleTestThing("Deadly Thing", singleTextProp("[[Single Text]]"))
      }
      implicit val s = new TSpace

      pql("""[[Deadly Thing -> Single Text]]""") should include("Too many levels of calls")
    }
  }
  
  "Tag / Name Literals" should {
    "work if the name is a defined Thing" in {
      implicit val s = new CDSpace
      
      pql("""[[Mandatory Fun]]""") should equal(linkText(s.mandatoryFun))
    }
    
    "work if the name is a used but undefined Tag" in {
      implicit val s = new CDSpace
      
      pql("""[[Weird]]""") should equal (unknownName("Weird"))
    }

    // Note: this behavior changed with QL.9v5kelv:
    "work if the name is neither a Thing nor a Tag" in {
      implicit val s = new CDSpace
      
      pql("""[[Some Other Genre]]""") should equal (unknownName("Some Other Genre"))
    }
    
    "work if the name is neither a Thing nor a Tag, but is marked explicitly as a Tag" in {
      implicit val s = new CDSpace
      
      pql("""[[`Some Other Genre`]]""") should equal (unknownName("Some Other Genre"))
    }
  }
  
  "OID Literals" should {
    // QI.7w4g8tv: parser crashes on ._self:
    "fail appropriately if the OID doesn't parse" in {
      implicit val s = new CDSpace
      
      pql("""[[._something]]""") should equal (expectedWarning("QL.unknownThingId"))      
    }

    // QI.7w4geqa: extra dot causes things to hang:
    "cope reasonably if the OID syntax is broken" in {
      implicit val s = new CDSpace

      pql("""[[..123456]]""") should equal (expectedWarning("QL.unknownThingId"))
    }
  }
  
  "Errors" should {
    "be placed at the appropriate place in the containing text" in {
      implicit val s = commonSpace
      
      pql("""Before the error [[Some Other Genre -> _oid]] After the error""") should
        equal (s"""Before the error ${expectedWarning("Func.notThing")} After the error""")
    }
  }
  
  "Operators" should {
    "work with simple truth values" in {
      implicit val s = commonSpace
      
      pql("""[[_if(True | False, ""yes"", ""no"")]]""") should equal ("yes")
      pql("""[[_if(True & False, ""yes"", ""no"")]]""") should equal ("no")
      pql("""[[!True]]""") should equal ("false")
      pql("""[[!False]]""") should equal ("true")
      pql("""[[_if(!(True | False), ""yes"", ""no"")]]""") should equal ("no")
      pql("""[[_if(!(True & False), ""yes"", ""no"")]]""") should equal ("yes")
      pql("""[[_if(!True & False, ""yes"", ""no"")]]""") should equal ("no")
    }
    
    "work with context" in {
      class TSpace extends CommonSpace {
        val singleBoolProp = new TestProperty(Core.YesNoType, ExactlyOne, "Single Boolean")
        
        val truthy = new SimpleTestThing("Truthy", singleBoolProp(true))
        val falsy = new SimpleTestThing("Falsey", singleBoolProp(false))
      }
      implicit val s = new TSpace
      
      pql("""[[Truthy -> _if(Single Boolean | False, ""yes"", ""no"")]]""") should equal ("yes")
      pql("""[[Truthy -> _if(Single Boolean & False, ""yes"", ""no"")]]""") should equal ("no")
      pql("""[[Falsey -> _if(Single Boolean | False, ""yes"", ""no"")]]""") should equal ("no")
      pql("""[[Falsey -> _if(Single Boolean & False, ""yes"", ""no"")]]""") should equal ("no")
      pql("""[[Falsey -> _if(!Single Boolean & True, ""yes"", ""no"")]]""") should equal ("yes")
    }
    
    "work with parens" in {
      implicit val s = commonSpace
      
      pql("""[[_if(True & (True & True), ""yes"", ""no"")]]""") should equal ("yes")
      pql("""[[_if((True & True) & True, ""yes"", ""no"")]]""") should equal ("yes")
      pql("""[[_if(!(True & True) & True, ""yes"", ""no"")]]""") should equal ("no")
    }
  }
  
  "Text Block Literals" should {
    "work in the trivial case" in {
      implicit val s = commonSpace
      
      pql("""[[``````]]""") should equal ("``````")
    }
    
    "work with some simple text" in {
      implicit val s = commonSpace
      
      pql("""[[```foo```]]""") should equal ("```foo```")
    }
    
    "work with some actual code" in {
      implicit val s = commonSpace
      
      pql("""[[```[[Foo -> Bar]]```]]""") should equal ("```[[Foo -> Bar]]```")
      pql("""[[```
            |[[Foo -> Bar]]
            |```]]""".stripReturns) should equal ("```\n[[Foo -> Bar]]\n```")
    }
  }
}
