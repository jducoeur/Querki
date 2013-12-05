package querki.logic

import models.system.ExactlyOne
import models.system.YesNoType

import querki.test._

class LogicTests extends QuerkiTests {
  "_if" should {
    "work correctly with True and False predicates" in {
      processQText(commonThingAsContext(_.sandbox), """[[_if(True, ""Yes"", ""No"")]]""") should 
        equal ("""Yes""")      
      processQText(commonThingAsContext(_.sandbox), """[[_if(False, ""Yes"", ""No"")]]""") should 
        equal ("""No""")      
    }
  }
  
  "_equals" should {
    "compare correctly with True and False" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(YesNoType, ExactlyOne, "Boolean Prop")
        
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
}
