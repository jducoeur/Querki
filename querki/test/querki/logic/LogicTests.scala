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
    
    "be able to test Name against Text" in {
      class TSpace extends CommonSpace {
        val myThing = new SimpleTestThing("My Thing", optTextProp("Trivial"))
      }
      implicit val s = new TSpace
      
      pql("""[[Trivial -> _equals(Name, ""Trivial"")]]""") should equal ("true")
      pql("""[[Trivial -> _equals(Name, ""Floob"")]]""") should equal ("false")
      pql("""[[Trivial -> _equals(Name, My Thing -> My Optional Text)]]""") should equal ("true")
      pql("""[[Trivial -> _equals(""Trivial"", Name)]]""") should equal ("true")
      pql("""[[Trivial -> _equals(""Floob"", Name)]]""") should equal ("false")
      pql("""[[Trivial -> _equals(My Thing -> My Optional Text, Name)]]""") should equal ("true")
    }
    
    "fail if there is a hard Type mismatch" in {
      class TSpace extends CommonSpace {
        val intProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        val myThing = new SimpleTestThing("My Thing", intProp(5))
      }
      implicit val s = new TSpace
      
      pql("""[[Trivial -> _equals(Name, My Thing -> My Num)]]""") should equal ("{{_warning:Logic.equals.typeMismatch}}")      
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
