package querki.logic

import querki.test._

class LogicTests extends QuerkiTests {
  // === _and ===
  "_and" should {
    "work correctly with True and False" in {
      implicit val s = commonSpace
      
      pql("""[[_and(False, True, False, True)]]""") should equal ("false")
      pql("""[[_and(False, False, False)]]""") should equal ("false")
      pql("""[[_and(False)]]""") should equal ("false")
      pql("""[[_and(True)]]""") should equal ("true")
      pql("""[[_and(True, True, True)]]""") should equal ("true")
    }
  }
  
  // === _equals ===
  "_equals" should {
    "compare correctly with True and False" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(Core.YesNoType, ExactlyOne, "Boolean Prop")
        
        val theModel = new SimpleTestThing("Sorting Model")
        val trueThing = new TestThing("Thing 1", theModel, boolProp(true))
        val falseThing = new TestThing("Thing 2", theModel, boolProp(false))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.trueThing), """[[_if(_equals(Boolean Prop, True), ""Yes"", ""No"")]]""") should 
        equal ("""Yes""")      
      processQText(thingAsContext[TSpace](space, _.trueThing), """[[_if(_equals(Boolean Prop, False), ""Yes"", ""No"")]]""") should 
        equal ("""No""")      
      processQText(thingAsContext[TSpace](space, _.falseThing), """[[_if(_equals(Boolean Prop, True), ""Yes"", ""No"")]]""") should 
        equal ("""No""")      
      processQText(thingAsContext[TSpace](space, _.falseThing), """[[_if(_equals(Boolean Prop, False), ""Yes"", ""No"")]]""") should 
        equal ("""Yes""")      
    }
  }
  
  // === _if ===
  "_if" should {
    "work correctly with True and False predicates" in {
      processQText(commonThingAsContext(_.sandbox), """[[_if(True, ""Yes"", ""No"")]]""") should 
        equal ("""Yes""")      
      processQText(commonThingAsContext(_.sandbox), """[[_if(False, ""Yes"", ""No"")]]""") should 
        equal ("""No""")      
    }
  }
  
  // === _or ===
  "_or" should {
    "work correctly with True and False" in {
      implicit val s = commonSpace
      
      pql("""[[_or(False, True, False, True)]]""") should equal ("true")
      pql("""[[_or(False, False, False)]]""") should equal ("false")
      pql("""[[_or(False)]]""") should equal ("false")
      pql("""[[_or(True)]]""") should equal ("true")
    }
  }
}
