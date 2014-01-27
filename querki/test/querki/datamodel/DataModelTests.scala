package querki.datamodel

import querki.test._

class DataModelTests extends QuerkiTests {
  // === _hasProperty ===
  "_hasProperty" should {
    "produce true iff the Thing has the Property" in {
      processQText(commonThingAsContext(_.instance), """[[_hasProperty(My Optional Text._self)]]""") should 
        equal ("""true""")      
    }
    
    "produce false iff the Thing doesn't have the Property" in {
      processQText(commonThingAsContext(_.withDisplayName), """[[_hasProperty(My Optional Text._self)]]""") should 
        equal ("""false""")      
    }
    
    "error if there are no parameters" in {
      processQText(commonThingAsContext(_.withDisplayName), """[[_hasProperty]]""") should 
        equal (expectedWarning("Func.missingParam"))      
    }
    
    "error if it receives a non-Thing" in {
      processQText(commonThingAsContext(_.instance), """[[My Optional Text -> _hasProperty(My Optional Text._self)]]""") should 
        equal (expectedWarning("Func.notThing"))      
    }
    
    "error if the parameter isn't a Thing" in {
      processQText(commonThingAsContext(_.instance), """[[_hasProperty(My Optional Text)]]""") should 
        equal (expectedWarning("Func.paramNotThing"))      
    }
    
    "process a List of Things, saying whether they all have the Property" in {
      class TSpace extends CommonSpace {
        val myProp = new TestProperty(TextType, ExactlyOne, "My Text Prop")
        val linkee1 = new SimpleTestThing("Linkee 1", myProp("hello"))
        val linkee2 = new SimpleTestThing("Linkee 2")
        val linkee3 = new SimpleTestThing("Linkee 3", myProp("there"))
        val linkee4 = new SimpleTestThing("Linkee 4")
        val linker = new SimpleTestThing("Linker", listLinksProp(linkee1, linkee2, linkee3, linkee4))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.linker)), """[[My List of Links -> _hasProperty(My Text Prop._self) -> _commas]]""") should
        equal ("""true, false, true, false""")      
    }
  }
  
  // === _is ===
  "_is" should {
    "successfully check an incoming Thing" in {
      processQText(commonThingAsContext(_.instance), """[[_is(My Instance)]]""") should 
        equal ("""true""")
    }
    
    "work inside _if" in {
      processQText(commonThingAsContext(_.instance), """[[_if(_is(My Instance), ""hello"")]]""") should 
        equal ("""hello""")      
    }
    
    "correctly fail the wrong Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_is(My Instance)]]""") should 
        equal ("""false""")
    }

    "work on a Link Property" in {
      class TSpace extends CommonSpace {
        val linkee = new SimpleTestThing("Linkee")
        val linker = new SimpleTestThing("Linker", singleLinkProp(linkee))
        val other = new SimpleTestThing("Other")
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.linker)), """[[_if(Single Link -> _is(Linkee), ""Yes"", ""No"")]]""") should
        equal ("""Yes""")
      
      processQText(thingAsContext[TSpace](space, (_.linker)), """[[_if(Single Link -> _is(Other), ""Yes"", ""No"")]]""") should
        equal ("""No""")
    }
    
    // Edge case, but this ought to work:
    "work with a received List" in {
      class TSpace extends CommonSpace {
        val linkee1 = new SimpleTestThing("Linkee 1")
        val linkee2 = new SimpleTestThing("Linkee 2")
        val linkee3 = new SimpleTestThing("Linkee 3")
        val linker = new SimpleTestThing("Linker", listLinksProp(linkee1, linkee2, linkee3, linkee2))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.linker)), """[[My List of Links -> _is(Linkee 2) -> _commas]]""") should
        equal ("""false, true, false, true""")
    }
  }
}