package querki.collections

import querki.test._

class CollectionRenderingTests extends QuerkiTests {
  "Lists" should {
    "render numbers correctly" in {
      class TSpace extends CommonSpace {
        val myProp = new TestProperty(Core.IntType, QList, "My List")
        val thing1 = new SimpleTestThing("Thing 1", myProp())
        val thing2 = new SimpleTestThing("Thing 2", myProp(42))
        val thing3 = new SimpleTestThing("Thing 3", myProp(5, 10, 15, 20))
      }
      implicit val s = new TSpace
      
      pql("""The result is: [[Thing 1 -> My List -> _bulleted]]""") should 
        equal ("""The result is: """)
      pql("""The result is: [[Thing 2 -> My List -> _bulleted]]""") should 
        equal ("""The result is: 
            |* 42""".stripReturns)
      pql("""The result is: [[Thing 3 -> My List -> _bulleted]]""") should 
        equal ("""The result is: 
            |* 5
            |* 10
            |* 15
            |* 20""".stripReturns)
    }
    
    "render text correctly" in {
      class TSpace extends CommonSpace {
        val myProp = new TestProperty(Core.TextType, QList, "My List")
        val thing1 = new SimpleTestThing("Thing 1", myProp())
        val thing2 = new SimpleTestThing("Thing 2", myProp("42"))
        val thing3 = new SimpleTestThing("Thing 3", myProp("5", "10", "15", "20"))
      }
      implicit val s = new TSpace
      
      pql("""The result is: [[Thing 1 -> My List -> _bulleted]]""") should 
        equal ("""The result is: """)
      // This line is the unit test for bug .3y2852x:
      pql("""The result is: [[Thing 2 -> My List -> _bulleted]]""") should 
        equal ("""The result is: 
            |* 42""".stripReturns)
      pql("""The result is: [[Thing 3 -> My List -> _bulleted]]""") should 
        equal ("""The result is: 
            |* 5
            |* 10
            |* 15
            |* 20""".stripReturns)
    }
  }
}