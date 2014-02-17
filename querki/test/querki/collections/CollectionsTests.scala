package querki.collections

import querki.test._

class CollectionsTests extends QuerkiTests {
  // === _count ===
  "_count" should {
    "work normally with a list input" in {
      implicit val s = new CDSpace
      
      pql("""[[Blackmores Night -> Artists._refs -> _count]]""") should
        equal("3")
    }
    
    "work with an empty input" in {
      implicit val s = new CDSpace
      
      pql("""[[Whitney Houston -> Artists._refs -> _count]]""") should
        equal("0")      
    }
  }
  
  // === _foreach ===
  "_foreach" should {
    "work normally" in {
      lazy val QLType = Basic.QLType
      
      class TSpace extends CommonSpace {
        val myQLProp = new TestProperty(QLType, ExactlyOne, "My Method")
        val listProp = new TestProperty(LinkType, QList, "List of Links")
        
        val thing1 = new SimpleTestThing("Thing 1")
        val thing2 = new SimpleTestThing("Thing 2")
        val thing3 = new SimpleTestThing("Thing 3")
        
        val myThing = new SimpleTestThing("My Thing",
            myQLProp("""""I got:[[$_context]]"""""),
            listProp(thing1, thing2, thing3))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> List of Links -> _foreach(My Thing.My Method)]]""") should
        equal("""
            |I got:[Thing 1](Thing-1)
            |I got:[Thing 2](Thing-2)
            |I got:[Thing 3](Thing-3)""".stripReturns)
    }
  }
  
  // === _isEmpty ===
  "_isEmpty" should {
    "work correctly in dotted position" in {
      implicit val s = new CDSpace
      
      pql("""[[Classical Randomness -> Artists._isEmpty]]""") should
        equal("true")
      pql("""[[Flood -> Artists._isEmpty]]""") should
        equal("false")
    }
    
    "work correctly with received context" in {
      implicit val s = new CDSpace
      
      pql("""[[Classical Randomness -> Artists -> _isEmpty]]""") should
        equal("true")
      pql("""[[Flood -> Artists -> _isEmpty]]""") should
        equal("false")      
    }
  }
  
  // === _isNonEmpty ===
  "_isNonEmpty" should {
    "work correctly in dotted position" in {
      implicit val s = new CDSpace
      
      pql("""[[Classical Randomness -> Artists._isNonEmpty]]""") should
        equal("false")
      pql("""[[Flood -> Artists._isNonEmpty]]""") should
        equal("true")
    }
    
    "work correctly with received context" in {
      implicit val s = new CDSpace
      
      pql("""[[Classical Randomness -> Artists -> _isNonEmpty]]""") should
        equal("false")
      pql("""[[Flood -> Artists -> _isNonEmpty]]""") should
        equal("true")      
    }
  }
  
  // === _reverse ===
  "_reverse" should {
    "work correctly with an ordinary list" in {
      implicit val space = new CDSpace
      
      pql("""[[My Favorites -> Favorite Artists -> Artists._refs -> _sort -> _reverse]]""") should
        equal (listOfLinkText(space.shadowOfTheMoon, space.ghostOfARose, space.flood, space.firesAtMight, space.factoryShowroom))      
    }
    
    "work with an empty input" in {
      implicit val space = new CDSpace
      
      pql("""[[Whitney Houston -> Artists._refs -> _sort -> _reverse]]""") should
        equal (listOfLinkText())      
    }
  }
}