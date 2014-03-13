package querki.tags

import querki.test._

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