package querki.tags

import querki.test._

class TagTests extends QuerkiTests {
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