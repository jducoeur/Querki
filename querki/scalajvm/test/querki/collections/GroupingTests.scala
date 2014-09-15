package querki.collections

import querki.test._

class GroupingTests extends QuerkiTests {
  "_groupBy" should {
    "work with a simple numeric key" in {
      class TSpace extends CommonSpace {
        val keyProp = new TestProperty(Core.IntType, ExactlyOne, "My Key")
        
        val myModel = new SimpleTestThing("Grouping Model", keyProp(0))
        
        val thing1 = new TestThing("Thing 1", myModel, keyProp(1))
        val thing2 = new TestThing("Thing 2", myModel, keyProp(12))
        val thing3 = new TestThing("Thing 3", myModel, keyProp(9))
        val thing4 = new TestThing("Thing 4", myModel, keyProp(12))
        val thing5 = new TestThing("Thing 5", myModel, keyProp(9))
        val thing6 = new TestThing("Thing 6", myModel, keyProp(1))
        val thing7 = new TestThing("Thing 7", myModel, keyProp(1))
        val thing8 = new TestThing("Thing 8", myModel, keyProp(9))
        val thing9 = new TestThing("Thing 9", myModel, keyProp(12))
      }
      implicit val s = new TSpace
      
      pql("""[[Grouping Model._instances -> _groupBy(My Key) -> ""
          |Key: [[_groupKey]] [[_groupElements -> _sort]]""]]""".stripReturns) should 
        equal(s"""
          |
          |Key: 1 ${listOfLinkText(s.thing1, s.thing6, s.thing7)}
          |
          |Key: 9 ${listOfLinkText(s.thing3, s.thing5, s.thing8)}
          |
          |Key: 12 ${listOfLinkText(s.thing2, s.thing4, s.thing9)}""".stripReturns)
    }
  }

}