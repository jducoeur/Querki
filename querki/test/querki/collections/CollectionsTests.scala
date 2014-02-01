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