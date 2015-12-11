package querki.datamodel

import querki.test._

import querki.spaces.ThingChangeRequest
import querki.types.{ModelTypeDefiner, SimplePropertyBundle}

class DataModelTests extends QuerkiTests {
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  lazy val Tags = interface[querki.tags.Tags]
  
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
      val actualProps = SpaceChangeManager.thingChanges(ThingChangeRequest(s.state, Some(s.myModel.id), None, s.myInstance.props, s.myInstance.props.keys.toSeq)).newProps

      // In other words, myProp is being copied into the actual properties, but otherProp is not:
      assert(actualProps.contains(s.myProp.id))
      assert(!actualProps.contains(s.otherProp.id))
    }
  }
  
  // === _asType ===
  "_asType" should {
    "convert one PlainText OID to a Link" in {
      class TSpace extends CommonSpace {
        val plaintext = sandbox.id.toString
        val textProp = new TestProperty(Basic.PlainTextType, ExactlyOne, "My Text Prop")
        val myThing = new SimpleTestThing("Test Thing", textProp(plaintext))
      }
      implicit val s = new TSpace
      
      pql("""[[Test Thing -> My Text Prop -> _asType(Link Type)]]""") should
        equal (linkText(s.sandbox))
    }
  }
  
  // === _currentSpace ===
  "_currentSpace" should {
    "return the current Space" in {
      implicit val s = commonSpace
      pql("""[[_currentSpace]]""") should equal (linkText(commonState))
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
        equal (expectedWarning("Func.paramWrongType"))      
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
      
      processQText(thingAsContext[TSpace](space, (_.instancesModel)), """[[_instances]]""") should 
        equal (listOfLinkText(space.instance1, space.instance2, space.instance3))
    }
    
    "list no instances of an empty Model" in {
      class TSpace extends CommonSpace {
        val instancesModel = new SimpleTestThing("Model with no Instances", Core.IsModelProp(true))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, (_.instancesModel)), """[[_instances]]""") should 
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
      
      processQText(thingAsContext[TSpace](space, (_.instancesModel)), """[[_instances]]""") should 
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
      
      processQText(thingAsContext[TSpace](space, (_.linker)), """[[My List of Links -> _instances]]""") should 
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
      pql("""[[My Optional URL._self -> _isDefined]]""") should equal ("true")
    }
    
    "work in received position with something that doesn't exist" in {
      implicit val s = commonSpace
      pql("""[[Floob -> _isDefined]]""") should equal ("false")
    }
    
    "work on a Tag Set" in {
      class TSpace extends CommonSpace {
        val tagSetProp = new TestProperty(Tags.TagSetType, Core.QSet, "My Tag Set")
        
        val myThing = new SimpleTestThing("My Test Thing", tagSetProp("My Instance", "floobity", "Trivial"))
      }
      implicit val s = new TSpace
      pql("""[[My Tag Set._tagsForProperty -> _filter(_isDefined)]]""") should equal(listOfLinkText(s.instance, s.trivialThing))
    }
  }
  
  // === _kind ===
  "_kind" should {
    "work with the common Types" in {
      implicit val s = commonSpace
      
      pql("[[My Model -> _kind]]") should equal ("0")
      pql("[[Text Type -> _kind]]") should equal ("1")
      pql("[[My List of Links._self -> _kind]]") should equal ("2")
      pql("[[Test Space -> _kind]]") should equal ("3")
      pql("[[Optional -> _kind]]") should equal ("4")
    }
  }
  
  "_model" should {
    "work for an Instance" in {
      implicit val s = commonSpace
      pql("""[[My Instance -> _model]]""") should equal(linkText(s.testModel))
    }
    
    "work for a Model" in {
      implicit val s = commonSpace
      pql("""[[My Model -> _model]]""") should equal(linkText(Basic.SimpleThing))
    }
    
    "work for a list of Things" in {
      class TSpace extends CommonSpace {
        val thingWithList = new SimpleTestThing("Thing With List", listLinksProp(instance, testModel))
      }
      implicit val s = new TSpace
      pql("""[[Thing With List -> My List of Links -> _model]]""") should equal(listOfLinkText(s.testModel, Basic.SimpleThing))
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
    
    // Test for Issue .3y285gi
    "find references from inside Model Types" in {
      class TSpace extends CommonSpace with ModelTypeDefiner {
        val linkModel = new SimpleTestThing("Link Model")
        val link1 = new TestThing("Link 1", linkModel)
        val link2 = new TestThing("Link 2", linkModel)
        
        val linkProp = new TestProperty(LinkType, QSet, "My Link Prop")
        
        val subModel = new SimpleTestThing("SubModel", linkProp())
        val propOfModelType = TestModelProperty("Complex Prop", subModel, Optional)
        
        val midModel = new SimpleTestThing("MidModel", propOfModelType())
        val propOfMidModelType = TestModelProperty("Deep Prop", midModel, Optional)
        
        val topModel = new SimpleTestThing("My Model", propOfModelType(), propOfMidModelType(), linkProp())
        val instance1 = new TestThing("Thing 1", topModel, linkProp(link1))
        val instance2 = new TestThing("Thing 2", topModel, propOfModelType(SimplePropertyBundle(linkProp(link2))))
        val instance3 = new TestThing("Thing 3", topModel,
            propOfMidModelType(SimplePropertyBundle(propOfModelType(SimplePropertyBundle(linkProp(link2))))))
      }
      implicit val s = new TSpace
      
      pql("""[[Link 1 -> My Link Prop._refs]]""") should
        equal(listOfLinkText(s.instance1))
      pql("""[[Link 2 -> My Link Prop._refs -> _sort]]""") should
        equal(listOfLinkText(s.instance2, s.instance3))
    }
  }
}