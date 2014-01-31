package querki.collections

import querki.test._

class CollectionsTests extends QuerkiTests {
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
}