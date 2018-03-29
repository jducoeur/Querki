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
    
    "work correctly with empty QL parameters" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(Core.YesNoType, ExactlyOne, "Boolean Prop")
        
        val theThing = new SimpleTestThing("My Thing")
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> _and(Boolean Prop, false)]]""") should equal ("false")
    }
    
    "work correct using the & operator" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(Core.YesNoType, ExactlyOne, "Boolean Prop")
        
        val theThing = new SimpleTestThing("My Thing")
        val otherThing = new SimpleTestThing("Other Thing", boolProp(true))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> Boolean Prop & false]]""") should equal ("false")
      pql("""[[Other Thing -> Boolean Prop & true]]""") should equal ("true")
    }
    
    "work with Lists as parameters" in {
      implicit val s = commonSpace
      pql("""[[_and(<true, true, true, true>)]]""") should equal ("true")      
      pql("""[[_and(<true, true, true, true>, <true, false>)]]""") should equal ("false")      
      pql("""[[_and(<true, true, false, true>, <true, true>)]]""") should equal ("false")      
      pql("""[[_and(<true, false, true, false>)]]""") should equal ("false")
    }
  }
    
  // === _divideBy ===
  "_divideBy" should {
    "work as expected" in {
      implicit val s = commonSpace
      
      pql("""[[8 -> _divideBy(3) -> ""__1.2__""]]""") should equal ("2.67")
      pql("""[[8 -> _divideBy(-3) -> ""__1.2__""]]""") should equal ("-2.67")
      pql("""[[3 -> +$three; 8 -> _divideBy($three) -> ""__1.2__""]]""") should equal ("2.67")      
    }
    
    "cope with mixed types" in {
      implicit val s = commonSpace
      
      pql("""[[3 -> _divideBy(2) -> +$onePointFive; 3 -> _divideBy($onePointFive)]]""") should equal ("2.0")
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
      
      pql("""[[Trivial -> _equals(Link Name, ""Trivial"")]]""") should equal ("true")
      pql("""[[Trivial -> _equals(Link Name, ""Floob"")]]""") should equal ("false")
      pql("""[[Trivial -> _equals(Link Name, My Thing -> My Optional Text)]]""") should equal ("true")
      pql("""[[Trivial -> _equals(""Trivial"", Link Name)]]""") should equal ("true")
      pql("""[[Trivial -> _equals(""Floob"", Link Name)]]""") should equal ("false")
      pql("""[[Trivial -> _equals(My Thing -> My Optional Text, Link Name)]]""") should equal ("true")
    }
    
    "fail if there is a hard Type mismatch" in {
      class TSpace extends CommonSpace {
        val intProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        val myThing = new SimpleTestThing("My Thing", intProp(5))
      }
      implicit val s = new TSpace
      
      pql("""[[Trivial -> _equals(Link Name, My Thing -> My Num)]]""") should equal ("{{_warning:Logic.equals.typeMismatch}}")      
    }
  }
  
  // === _greaterThan ===
  "_greaterThan" should {    
    "compare numbers correctly" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        
        val thing1 = new SimpleTestThing("Thing 1", numProp(3))
        val thing2 = new SimpleTestThing("Thing 2", numProp(3))
        val thing3 = new SimpleTestThing("Thing 3", numProp(12))
      }
      implicit val s = new TSpace
      
      pql("""[[_greaterThan(Thing 1 -> My Num, Thing 2 -> My Num)]]""") should equal ("false")
      pql("""[[_greaterThan(Thing 1 -> My Num, Thing 3 -> My Num)]]""") should equal ("false")
      pql("""[[_greaterThan(Thing 3 -> My Num, Thing 2 -> My Num)]]""") should equal ("true")
      
      pql("""[[_greaterThan(Thing 1 -> My Num, 1)]]""") should equal ("true")
      pql("""[[_greaterThan(Thing 1 -> My Num, 14)]]""") should equal ("false")
      
      pql("""[[Thing 1 -> My Num -> _greaterThan(4)]]""") should equal ("false")
    }  
    
    "compare strings correctly" in {
      implicit val s = commonSpace
      
      pql("""[[_greaterThan(""hello"", ""there"")]]""") should equal ("false")
      pql("""[[_greaterThan(""there"", ""hello"")]]""") should equal ("true")
    }
  }
  
  // === _if ===
  "_if" should {
    "work correctly with True and False predicates" in {
      implicit val s = new CommonSpace
      
      pql("""[[_if(True, ""Yes"", ""No"")]]""") should 
        equal ("""Yes""")      
      pql("""[[_if(False, ""Yes"", ""No"")]]""") should 
        equal ("""No""")      
    }
    
    "work correctly with a missing iffalse clause" in {
      implicit val s = new CommonSpace
      
      pql("""[[_if(True, ""Yes"", ""No"")]]""") should 
        equal ("""Yes""")      
      pql("""[[_if(False, ""Yes"")]]""") should 
        equal ("""""")
    }
    
    "work correctly with flags" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(Core.YesNoType, ExactlyOne, "My Bool")
        
        val flagTrue = new SimpleTestThing("Flag True", boolProp(true))
        val flagFalse = new SimpleTestThing("Flag False", boolProp(false))
        val flagMissing = new SimpleTestThing("Flag Missing")
      }
      implicit val s = new TSpace
      
      pql("""[[Flag True -> _if(My Bool, ""Yes"", ""No"")]]""") should
        equal ("Yes")
      pql("""[[Flag False -> _if(My Bool, ""Yes"", ""No"")]]""") should
        equal ("No")
      pql("""[[Flag Missing -> _if(My Bool, ""Yes"", ""No"")]]""") should
        equal ("No")
    }
    
    "propagate errors in the predicate" in {
      class TSpace extends CommonSpace {
        val intProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        val myThing = new SimpleTestThing("My Thing", intProp(5))
      }
      implicit val s = new TSpace
      
      pql("""[[_if(Trivial -> _equals(Link Name, My Thing -> My Num), ""hello"")]]""") should equal ("{{_warning:Logic.equals.typeMismatch}}")            
    }
    
    // Unit test for QI.7w4g86b
    "work correctly with missing parameters" in {
      implicit val s = commonSpace
      
      // Missing predicate, which is empty, which is false:
      pql("""[[_if(,""yes"",""no"")]]""") should equal ("no")
      // Missing true clause:
      pql("""[[_if(true,,""no"")]]""") should equal ("")
      pql("""[[_if(false,,""no"")]]""") should equal ("no")
      // Missing false clause:
      pql("""[[_if(true,""yes"",)]]""") should equal ("yes")
      pql("""[[_if(false,""yes"",)]]""") should equal ("")      
    }
  }
  
  // === _lessThan ===
  "_lessThan" should {    
    "compare numbers correctly" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, ExactlyOne, "My Num")
        
        val thing1 = new SimpleTestThing("Thing 1", numProp(3))
        val thing2 = new SimpleTestThing("Thing 2", numProp(3))
        val thing3 = new SimpleTestThing("Thing 3", numProp(12))
      }
      implicit val s = new TSpace
      
      pql("""[[_lessThan(Thing 1 -> My Num, Thing 2 -> My Num)]]""") should equal ("false")
      pql("""[[_lessThan(Thing 1 -> My Num, Thing 3 -> My Num)]]""") should equal ("true")
      
      pql("""[[_lessThan(Thing 1 -> My Num, 1)]]""") should equal ("false")
      pql("""[[_lessThan(Thing 1 -> My Num, 14)]]""") should equal ("true")
      
      pql("""[[Thing 1 -> My Num -> _lessThan(4)]]""") should equal ("true")
    }  
    
    "compare strings correctly" in {
      implicit val s = commonSpace
      
      pql("""[[_lessThan(""hello"", ""there"")]]""") should equal ("true")
      pql("""[[_lessThan(""there"", ""hello"")]]""") should equal ("false")
    }
  }
  
  // === _minus ===
  "_minus" should {
    "work as expected" in {
      implicit val s = commonSpace
      
      pql("""[[8 -> _minus(3)]]""") should equal ("5")
      pql("""[[8 -> _minus(-3)]]""") should equal ("11")
      pql("""[[3 -> +$three; 8 -> _minus($three)]]""") should equal ("5")      
    }
    
    "cope with mixed types" in {
      implicit val s = commonSpace
      
      pql("""[[3 -> _divideBy(2) -> +$onePointFive; 3 -> _minus($onePointFive)]]""") should equal ("1.5")
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
    
    "work correctly with empty QL parameters" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(Core.YesNoType, ExactlyOne, "Boolean Prop")
        
        val theThing = new SimpleTestThing("My Thing")
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> _or(Boolean Prop, true)]]""") should equal ("true")
    }
    
    "work correctly with the | operator" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(Core.YesNoType, ExactlyOne, "Boolean Prop")
        
        val falseThing = new SimpleTestThing("F Thing", boolProp(false))
        val falseThing2 = new SimpleTestThing("F Thing 2", boolProp(false))
        val trueThing = new SimpleTestThing("T Thing", boolProp(true))
      }
      implicit val s = new TSpace
      
      pql("""[[F Thing -> Boolean Prop | true]]""") should equal ("true")      
      pql("""[[F Thing -> Boolean Prop | false]]""") should equal ("false")
      pql("""[[(F Thing -> Boolean Prop) | (T Thing -> Boolean Prop)]]""") should equal ("true")
    }
    
    "combine in complex ways with &" in {
      class TSpace extends CommonSpace {
        val boolProp = new TestProperty(Core.YesNoType, ExactlyOne, "Boolean Prop")
        
        val falseThing = new SimpleTestThing("F Thing", boolProp(false))
        val falseThing2 = new SimpleTestThing("F Thing 2", boolProp(false))
        val trueThing = new SimpleTestThing("T Thing", boolProp(true))
      }
      implicit val s = new TSpace
      
      pql("""[[(F Thing -> Boolean Prop) & (T Thing -> Boolean Prop)]]""") should equal ("false")      
      pql("""[[((F Thing -> Boolean Prop) & (T Thing -> Boolean Prop))
             | (T Thing -> Boolean Prop)]]""") should equal ("true")      
    }
    
    "work with Lists as parameters" in {
      implicit val s = commonSpace
      pql("""[[_or(<true, true, true, true>)]]""") should equal ("true")      
      pql("""[[_or(<false, false, false, false>, <false, true>)]]""") should equal ("true")      
      pql("""[[_or(<false, false, true, false>, <false, false>)]]""") should equal ("true")      
      pql("""[[_or(<false, false, false, false>)]]""") should equal ("false")
    }    
  }
    
  // === _plus ===
  "_plus" should {
    "work as expected" in {
      implicit val s = commonSpace
      
      pql("""[[8 -> _plus(3)]]""") should equal ("11")
      pql("""[[8 -> _plus(-3)]]""") should equal ("5")
      pql("""[[3 -> +$three; 8 -> _plus($three)]]""") should equal ("11")      
    }
    
    "cope with mixed types" in {
      implicit val s = commonSpace
      
      pql("""[[3 -> _divideBy(2) -> +$onePointFive; 3 -> _plus($onePointFive)]]""") should equal ("4.5")
    }
  }
    
  // === _times ===
  "_times" should {
    "work as expected" in {
      implicit val s = commonSpace
      
      pql("""[[8 -> _times(3)]]""") should equal ("24")
      pql("""[[8 -> _times(-3)]]""") should equal ("-24")
      pql("""[[3 -> +$three; 8 -> _times($three)]]""") should equal ("24")      
    }
    
    "cope with mixed types" in {
      implicit val s = commonSpace
      
      // This is the common and problematic case:
      pql("""[[3 -> _divideBy(2) -> _times(3)]]""") should equal ("4.5")
    }
  }
}
