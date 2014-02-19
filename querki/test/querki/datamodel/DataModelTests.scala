package querki.datamodel

import querki.test._

import querki.spaces.ThingChangeRequest

class DataModelTests extends QuerkiTests {
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  
  // === Copy Into Instances ===
  "Copy Into Instances" should {
    "work normally" in {
      class TSpace extends CommonSpace {
        val myProp = new TestProperty(TextType, Optional, "My Text Prop", DataModelAccess.CopyIntoInstances(true))
        val otherProp = new TestProperty(TextType, Optional, "Other Text Prop")
        
        val myModel = new SimpleTestThing("My Model", myProp("Hello"), otherProp("Yay!"))
        
        val myInstance = new TestThing("My Instance", myModel)
      }
      implicit val s = new TSpace
      
      assert(!s.myInstance.props.contains(s.myProp.id))
      assert(!s.myInstance.props.contains(s.otherProp.id))
      
      // TODO: this is a pretty low-level test. Once we actually have unit testing working for the Space Actor,
      // we should test that way, to get a more realistic functional test:
      val actualProps = SpaceChangeManager.thingChanges(ThingChangeRequest(s.state, Some(s.myModel.id), None, s.myInstance.props)).newProps

      // In other words, myProp is being copied into the actual properties, but otherProp is not:
      assert(actualProps.contains(s.myProp.id))
      assert(!actualProps.contains(s.otherProp.id))
    }
  }
  
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
  
  // === _instances ===
  "_instances" should {
    "list the single instance of a Model" in {
      processQText(commonThingAsContext(_.testModel), """[[_instances -> _commas]]""") should 
        equal ("""[My Instance](My-Instance)""")
    }
    
    "list several instances of a Model" in {
      class TSpace extends CommonSpace {
        val instancesModel = new SimpleTestThing("Model with Instances", Core.IsModelProp(true))
        val instance1 = new TestThing("Instance 1", instancesModel)
        val instance2 = new TestThing("Instance 2", instancesModel)
        val instance3 = new TestThing("Instance 3", instancesModel)
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.instancesModel)), """[[_instances -> _sort]]""") should 
        equal (listOfLinkText(space.instance1, space.instance2, space.instance3))
    }
    
    "list no instances of an empty Model" in {
      class TSpace extends CommonSpace {
        val instancesModel = new SimpleTestThing("Model with no Instances", Core.IsModelProp(true))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.instancesModel)), """[[_instances -> _sort]]""") should 
        equal ("""""")
    }
    
    "list instances through SubModels" in {
      class TSpace extends CommonSpace {
        val instancesModel = new SimpleTestThing("Model with Instances", Core.IsModelProp(true))
        val subModel = new TestThing("Submodel", instancesModel, Core.IsModelProp(true))
        val instance1 = new TestThing("Instance 1", instancesModel)
        val instance2 = new TestThing("Instance 2", subModel)
        val instance3 = new TestThing("Instance 3", instancesModel)
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.instancesModel)), """[[_instances -> _sort]]""") should 
        equal (listOfLinkText(space.instance1, space.instance2, space.instance3))
    }
    
    "work in dotted position" in {
      processQText(commonThingAsContext(_.sandbox), """[[My Model._instances -> _commas]]""") should 
        equal ("""[My Instance](My-Instance)""")
    }
    
    "cope with multiple received Models" in {
      class TSpace extends CommonSpace {
        val instancesModel = new SimpleTestThing("Model with Instances", Core.IsModelProp(true))
        val model2 = new SimpleTestThing("Model 2", Core.IsModelProp(true))
        
        val instance1 = new TestThing("Instance 1", instancesModel)
        val instance2 = new TestThing("Instance 2", model2)
        val instance3 = new TestThing("Instance 3", instancesModel)
        
        val linker = new SimpleTestThing("Linker", listLinksProp(instancesModel, model2))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.linker)), """[[My List of Links -> _instances -> _sort]]""") should 
        equal (listOfLinkText(space.instance1, space.instance2, space.instance3))
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
  
  // === _isDefined
  "_isDefined" should {
    "work in dotted position with something that does exist" in {
      implicit val s = commonSpace
      pql("""[[My Optional URL._isDefined]]""") should equal ("true")
    }
    
    // Note that QL syntax only allows dotting of names that actually exist; otherwise, the rest of
    // the stage gets ignored. This *might* change, but for now let's test the actual behaviour:
    "can not work in dotted position with something that doesn't exist" in {
      implicit val s = commonSpace
      pql("""[[Floob._isDefined]]""") should equal ("{{_unknownName:[Floob](Floob)}}")
    }
    
    "work in received position with something that does exist" in {
      implicit val s = commonSpace
      pql("""[[My Optional URL -> _isDefined]]""") should equal ("true")
    }
    
    "work in received position with something that doesn't exist" in {
      implicit val s = commonSpace
      pql("""[[Floob -> _isDefined]]""") should equal ("false")
    }
  }
  
  // === _refs ===
  "_refs" should {
    "find a bunch of ordinary Links" in {
      implicit val space = new CDSpace
      
      pql("""[[They Might Be Giants -> Artists._refs -> _sort]]""") should
        equal (listOfLinkText(space.factoryShowroom, space.flood))        
    }
    
    "cope with an empty received list" in {
      implicit val space = new CDSpace
      
      pql("""[[Whitney Houston -> Artists._refs -> _sort]]""") should
        equal (listOfLinkText())        
    }
    
    "cope with a list of inputs" in {
      implicit val space = new CDSpace
      
      pql("""[[My Favorites -> Favorite Artists -> Artists._refs -> _sort]]""") should
        equal (listOfLinkText(space.factoryShowroom, space.firesAtMight, space.flood, space.ghostOfARose, space.shadowOfTheMoon))        
    }
  }
}