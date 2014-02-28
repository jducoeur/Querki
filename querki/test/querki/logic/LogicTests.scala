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
    
    "compare numbers correctly" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        
        val thing1 = new SimpleTestThing("Thing 1", numProp(3))
        val thing2 = new SimpleTestThing("Thing 2", numProp(3))
        val thing3 = new SimpleTestThing("Thing 3", numProp(12))
      }
      implicit val s = new TSpace
      
      pql("""[[_equals(Thing 1 -> My Num, Thing 2 -> My Num)]]""") should equal ("true")
      pql("""[[_equals(Thing 1 -> My Num, Thing 3 -> My Num)]]""") should equal ("false")
      
      pql("""[[_equals(Thing 1 -> My Num, 3)]]""") should equal ("true")
      pql("""[[_equals(Thing 1 -> My Num, 14)]]""") should equal ("false")
    }
    
    "be able to receive the first value as context" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        
        val thing1 = new SimpleTestThing("Thing 1", numProp(3))
        val thing2 = new SimpleTestThing("Thing 2", numProp(3))
        val thing3 = new SimpleTestThing("Thing 3", numProp(12))
      }
      implicit val s = new TSpace
      
      pql("""[[Thing 1 -> My Num -> _equals(Thing 2 -> My Num)]]""") should equal ("true")
      pql("""[[Thing 1 -> My Num -> _equals(Thing 3 -> My Num)]]""") should equal ("false")
      
      pql("""[[Thing 1 -> My Num -> _equals(3)]]""") should equal ("true")
      pql("""[[Thing 1 -> My Num -> _equals(14)]]""") should equal ("false")
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
    
    "propagate errors in the predicate" in {
      class TSpace extends CommonSpace {
        val intProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        val myThing = new SimpleTestThing("My Thing", intProp(5))
      }
      implicit val s = new TSpace
      
      pql("""[[_if(Trivial -> _equals(Name, My Thing -> My Num), ""hello"")]]""") should equal ("{{_warning:Logic.equals.typeMismatch}}")            
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
