package querki.tags

import querki.test._

import querki.types.SimplePropertyBundle

class TagTests extends QuerkiTests {
  lazy val Tags = interface[querki.tags.Tags]
  
  // === _resolveTags ===
  "_resolveTags" should {
    "work normally" in {
      class TSpace extends CommonSpace {
        val listOfTags = new TestProperty(Tags.NewTagSetType, QList, "List of Tags")
        val oneTag = new TestProperty(Tags.NewTagSetType, ExactlyOne, "One Tag")
        
        val thing1 = new SimpleTestThing("Thing 1")
        val thing3 = new SimpleTestThing("Thing 3")
        
        val myThing = new SimpleTestThing("My Thing",
            listOfTags("Thing 1", "Thing 2", "Thing 3"),
            oneTag("Thing 1"))
        val unresolvedThing = new SimpleTestThing("Unresolved",
            listOfTags("Floobity", "Smurf", "fnord"))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> List of Tags -> _resolveTags]]""") should
        equal(listOfLinkText(s.thing1, s.thing3))
      pql("""[[My Thing -> One Tag -> _resolveTags]]""") should
        equal(linkText(s.thing1))
      
      pql("""[[Unresolved -> List of Tags -> _resolveTags]]""") should
        equal("")      
    }
  }
  
  // === _tagRefs ===
  "_tagRefs" should {
    "work with a single result" in {
      implicit val s = new CDSpace
      
      pql("""[[Weird -> _tagRefs]]""") should
        equal(listOfLinkText(s.tmbg))
    }
    
    "work with multiple results" in {
      implicit val s = new CDSpace
      
      pql("""[[Rock -> _tagRefs -> _sort]]""") should
        equal(listOfLinkText(s.blackmores, s.eurythmics, s.tmbg))
    }
    
    // Test for Issue .3y285gi
    "find references from inside Model Types" in {
      class TSpace extends CommonSpace {
        val tagProp = new TestProperty(Tags.NewTagSetType, QSet, "My Tag Prop")
        
        val subModel = new SimpleTestThing("SubModel", tagProp())
        val propOfModelType = TestModelProperty("Complex Prop", subModel, Optional)
        
        val midModel = new SimpleTestThing("MidModel", propOfModelType())
        val propOfMidModelType = TestModelProperty("Deep Prop", midModel, Optional)
        
        val topModel = new SimpleTestThing("My Model", propOfModelType(), propOfMidModelType(), tagProp())
        val instance1 = new TestThing("Thing 1", topModel, tagProp("Tag-1"))
        val instance2 = new TestThing("Thing 2", topModel, propOfModelType(SimplePropertyBundle(tagProp("Tag-2"))))
        val instance3 = new TestThing("Thing 3", topModel,
            propOfMidModelType(SimplePropertyBundle(propOfModelType(SimplePropertyBundle(tagProp("Tag-2"))))))
      }
      implicit val s = new TSpace
      
      pql("""[[Tag 1 -> _tagRefs]]""") should
        equal(listOfLinkText(s.instance1))
      pql("""[[Tag 2 -> _tagRefs -> _sort]]""") should
        equal(listOfLinkText(s.instance2, s.instance3))
    }
    
    // Test for Issue .3y286oo
    "work with a specified Property" in {
      class TSpace extends CommonSpace {
        val tagProp1 = new TestProperty(Tags.NewTagSetType, QSet, "First Tag Prop")
        val tagProp2 = new TestProperty(Tags.NewTagSetType, QSet, "Second Tag Prop")
        
        val targetThing = new SimpleTestThing("Target Thing")
        
        val source1 = new SimpleTestThing("Source 1", tagProp1("Target Thing"))
        val source2 = new SimpleTestThing("Source 2", tagProp2("Target Thing"))
        val source3 = new SimpleTestThing("Source 3", tagProp1("Target Thing"))
        val source4 = new SimpleTestThing("Source 4", tagProp2("Target Thing"))
      }
      implicit val s = new TSpace
      
      pql("""[[Target Thing -> _tagRefs -> _sort]]""") should
        equal(listOfLinkText(s.source1, s.source2, s.source3, s.source4))
      pql("""[[Target Thing -> First Tag Prop._tagRefs -> _sort]]""") should
        equal(listOfLinkText(s.source1, s.source3))
      pql("""[[Target Thing -> Second Tag Prop._tagRefs -> _sort]]""") should
        equal(listOfLinkText(s.source2, s.source4))
    }
  }
  
  // === _tagsForProperty ===
  "_tagsForProperty" should {
    "work for a simple Tag" in {
      implicit val s = new CDSpace
      
      pql("""[[Genres._tagsForProperty -> _sort -> _commas]]""") should
        equal("[Folk](Folk), [Pop](Pop), [Rock](Rock), [Weird](Weird)")
    }
    
    "work with a received context" in {
      implicit val s = new CDSpace
      
      pql("""[[Genres._self -> _tagsForProperty -> _sort -> _commas]]""") should
        equal("[Folk](Folk), [Pop](Pop), [Rock](Rock), [Weird](Weird)")      
    }
  }
}