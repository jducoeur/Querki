package querki.collections

import querki.test._

class CollectionsTests extends QuerkiTests {
  
  lazy val LargeTextType = Core.LargeTextType
  
  // === _concat ===
  "_concat" should {
    // NOTE: _concat() of Tags is tested over in TagTests
    
    "work through Properties" in {
      implicit val s = new CDSpace
      
      pql("""[[My Favorites -> _concat(Favorite Artists, Interesting Artists) -> _sort]]""") should
        equal(listOfLinkText(s.blackmores, s.eurythmics, s.tmbg))
    }
  }
  
  // === _contains ===
  "_contains" should {
    "work with simple numbers" in {
      class TSpace extends CommonSpace {
        val listProp = new TestProperty(Core.IntType, QList, "My Prop")
        val myThing = new SimpleTestThing("My Thing", listProp(5, 9, 92))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> My Prop -> _contains(9)]]""") should equal ("true")
      pql("""[[My Thing -> My Prop -> _contains(42)]]""") should equal ("false")
    }
    
    "work with links" in {
      class TSpace extends CommonSpace {
        val listProp = new TestProperty(Core.LinkType, QList, "My Prop")
        val thing1 = new SimpleTestThing("Thing 1")
        val thing2 = new SimpleTestThing("Thing 2")
        val thing3 = new SimpleTestThing("Thing 3")
        val myThing = new SimpleTestThing("My Thing", listProp(thing1, thing3))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> My Prop -> _contains(Thing 3)]]""") should equal ("true")
      pql("""[[My Thing -> My Prop -> _contains(Thing 2)]]""") should equal ("false")
    }
  }
  
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
  
  // === _drop ===
  "_drop" should {
    class TSpace extends CommonSpace {
      val thingWithList = new SimpleTestThing("My Thing", listTagsProp("One", "Two", "Three", "Four", "Five", "Six"))
      val thingWithEmptyList = new SimpleTestThing("My Empty Thing", listTagsProp())
    }
    
    "work normally" in {
      implicit val s = new TSpace
      
      pql("""[[My Thing -> My List of Tags -> _drop(3) -> _commas]]""") should
        equal ("[Four](Four), [Five](Five), [Six](Six)")
    }
    
    "produce an empty list if too far" in {
      implicit val s = new TSpace
      
      pql("""[[My Thing -> My List of Tags -> _drop(8) -> _commas]]""") should
        equal ("")      
    }
    
    "work with an empty List" in {
      implicit val s = new TSpace
      
      pql("""[[My Empty Thing -> My List of Tags -> _drop(2) -> _commas]]""") should
        equal ("")      
    }
  }
  
  // === _filter ===
  "_filter" should {
    "work with _equals" in {
      class testSpace extends CommonSpace {
        val linkTarget = new SimpleTestThing("Link Target")
        val pointer1 = new SimpleTestThing("Pointer 1", singleLinkProp(linkTarget))
        val pointer2 = new SimpleTestThing("Pointer 2", singleLinkProp(sandbox))
        val wrapper = new SimpleTestThing("Wrapper", listLinksProp(pointer1, pointer2))
      }
      
      processQText(thingAsContext[testSpace](new testSpace, _.wrapper), """[[My List of Links -> _filter(_equals(Single Link, Link Target))]]""") should
        equal ("\n[Pointer 1](Pointer-1)")      
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
    
    "work correctly with a parameter" in {
      implicit val s = new CDSpace
      
      pql("""[[Classical Randomness ->  _isEmpty(Artists)]]""") should
        equal("true")
      pql("""[[Flood -> _isEmpty(Artists)]]""") should
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
    
    "work correctly with a parameter" in {
      implicit val s = new CDSpace
      
      pql("""[[Classical Randomness -> _isNonEmpty(Artists)]]""") should
        equal("false")
      pql("""[[Flood -> _isNonEmpty(Artists)]]""") should
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
    
  // === _sort ===
  "_sort" should {
    class testSpace extends CommonSpace {
      val linkTarget = new SimpleTestThing("Link Target")
      val pointer1 = new SimpleTestThing("First Thing")
      val pointer2 = new SimpleTestThing("Another Thing")
      val wrapper = new SimpleTestThing("Wrapper", listLinksProp(linkTarget, pointer1, pointer2))
    }
    
    "sort by name by default" in {
      val space = new testSpace
      import space._
      processQText(thingAsContext[testSpace](space, _.wrapper), """[[My List of Links -> _sort]]""") should
        equal (listOfLinkText(pointer2, pointer1, linkTarget))
    }
    
    "sort by name with _desc" in {
      val space = new testSpace
      import space._
      
      processQText(thingAsContext[testSpace](space, _.wrapper), """[[My List of Links -> _sort(_desc(Name))]]""") should
        equal (listOfLinkText(linkTarget, pointer1, pointer2))
    }
    
    "sort by direct Display Text Properties" in {
      class TSpace extends CommonSpace {
        val sortingProp = new TestProperty(LargeTextType, Optional, "Prop to Sort")
        
        val theModel = new SimpleTestThing("Sorting Model")
        val thing1 = new TestThing("Thing 1", theModel, sortingProp("Kazam!"))
        val thing2 = new TestThing("Thing 2", theModel, sortingProp("Check!"))
        val thing3 = new TestThing("Thing 3", theModel, sortingProp("Wild!"))
        val thing4 = new TestThing("Thing 4", theModel, sortingProp("Floobity!"))
        val thing5 = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.theModel), """[[Sorting Model._instances -> _sort(Prop to Sort) -> ""[[Name]]: [[Prop to Sort]]""]]""") should
        equal ("\nThing 5: Alphabetical!\nThing 2: Check!\nThing 4: Floobity!\nThing 1: Kazam!\nThing 3: Wild!")      
    }
    
    "sort by indirect Display Text Properties" in {
      class TSpace extends CommonSpace {
        val sortingProp = new TestProperty(LargeTextType, Optional, "Prop to Sort")
        
        val theModel = new SimpleTestThing("Sorting Model")
        val thing1 = new TestThing("Thing 1", theModel, sortingProp("Kazam!"))
        val thing2 = new TestThing("Thing 2", theModel, sortingProp("Check!"))
        val thing3 = new TestThing("Thing 3", theModel, sortingProp("Wild!"))
        val thing4 = new TestThing("Thing 4", theModel, sortingProp("Floobity!"))
        val thing5 = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"))
        
        val refProp = new TestProperty(LinkType, ExactlyOne, "Reference")
        
        val refModel = new SimpleTestThing("Referring Model")
        val ref1 = new TestThing("Reference 1", refModel, refProp(thing1))
        val ref2 = new TestThing("Reference 2", refModel, refProp(thing2))
        val ref3 = new TestThing("Reference 3", refModel, refProp(thing3))
        val ref4 = new TestThing("Reference 4", refModel, refProp(thing4))
        val ref5 = new TestThing("Reference 5", refModel, refProp(thing5))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.theModel), """[[Referring Model._instances -> _sort(Reference -> Prop to Sort, ""Unknown"") -> ""[[Name]]: [[Reference -> Prop to Sort]]""]]""") should
        equal ("\nReference 5: Alphabetical!\nReference 2: Check!\nReference 4: Floobity!\nReference 1: Kazam!\nReference 3: Wild!")      
    }
    
    "work with several sort terms" in {
      class TSpace extends CommonSpace {
        val sortingProp = new TestProperty(LargeTextType, Optional, "Prop to Sort")
        val sortingProp2 = new TestProperty(Core.IntType, Optional, "Number Prop")
        val sortingProp3 = new TestProperty(TextType, Optional, "Another Prop")
        
        val theModel = new SimpleTestThing("Sorting Model")
        val thing1 = new TestThing("Thing 1", theModel, sortingProp("Kazam!"), sortingProp2(44), sortingProp3("z"))
        val thing2 = new TestThing("Thing 2", theModel, sortingProp("Check!"), sortingProp3("m"))
        val thing3 = new TestThing("Thing 3", theModel, sortingProp("Wild!"), sortingProp2(19))
        val thing4 = new TestThing("Thing 4", theModel, sortingProp("Floobity!"), sortingProp2(12), sortingProp3("m"))
        val thing5 = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"))
        val thing1a = new TestThing("Thing 1", theModel, sortingProp("Kazam!"), sortingProp2(12))
        val thing2a = new TestThing("Thing 2", theModel, sortingProp("Check!"), sortingProp2(44))
        val thing3a = new TestThing("Thing 3", theModel, sortingProp("Wild!"))
        val thing4a = new TestThing("Thing 4", theModel, sortingProp("Floobity!"), sortingProp2(99), sortingProp3("m"))
        val thing5a = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"), sortingProp2(44))
        val thing1b = new TestThing("Thing 1", theModel, sortingProp("Kazam!"), sortingProp2(44), sortingProp3("m"))
        val thing2b = new TestThing("Thing 2", theModel, sortingProp("Check!"), sortingProp3("a"))
        val thing3b = new TestThing("Thing 3", theModel, sortingProp("Wild!"), sortingProp3("m"))
        val thing4b = new TestThing("Thing 4", theModel, sortingProp("Floobity!"), sortingProp2(44), sortingProp3("n"))
        val thing5b = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"))
        val thing1c = new TestThing("Thing 1", theModel, sortingProp("Kazam!"), sortingProp2(44), sortingProp3("b"))
        val thing2c = new TestThing("Thing 2", theModel, sortingProp("Check!"), sortingProp3("z"))
        val thing3c = new TestThing("Thing 3", theModel, sortingProp("Wild!"), sortingProp3("c"))
        val thing4c = new TestThing("Thing 4", theModel, sortingProp("Floobity!"), sortingProp2(44), sortingProp3("m"))
        val thing5c = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"), sortingProp2(44))
      }
      implicit val s = new TSpace
      
      pql("""[[Sorting Model._instances -> _sort(Prop to Sort, Number Prop, Another Prop) -> ""[[Prop to Sort]], [[Number Prop]], [[Another Prop]]""]]""") should
        equal ("""
Alphabetical!, , 
Alphabetical!, , 
Alphabetical!, 44, 
Alphabetical!, 44, 
Check!, , a
Check!, , m
Check!, , z
Check!, 44, 
Floobity!, 12, m
Floobity!, 44, m
Floobity!, 44, n
Floobity!, 99, m
Kazam!, 12, 
Kazam!, 44, b
Kazam!, 44, m
Kazam!, 44, z
Wild!, , 
Wild!, , c
Wild!, , m
Wild!, 19, """.stripReturns)
    }
    
    
    "sort with empty references" in {
      class TSpace extends CommonSpace {
        val sortingProp = new TestProperty(LargeTextType, Optional, "Prop to Sort")
        
        val theModel = new SimpleTestThing("Sorting Model")
        val thing1 = new TestThing("Thing 1", theModel, sortingProp("Kazam!"))
        val thing2 = new TestThing("Thing 2", theModel, sortingProp("Check!"))
        val thing3 = new TestThing("Thing 3", theModel, sortingProp("Wild!"))
        val thing4 = new TestThing("Thing 4", theModel, sortingProp("Floobity!"))
        val thing5 = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"))
        
        val refProp = new TestProperty(LinkType, Optional, "Reference")
        
        val refModel = new SimpleTestThing("Referring Model")
        val ref1 = new TestThing("Reference 1", refModel, refProp(thing1))
        val ref1a = new TestThing("Reference 1a", refModel)
        val ref2 = new TestThing("Reference 2", refModel, refProp(thing2))
        val ref3 = new TestThing("Reference 3", refModel)
        val ref4 = new TestThing("Reference 4", refModel, refProp(thing4))
        val ref5 = new TestThing("Reference 5", refModel, refProp(thing5))
        val ref6 = new TestThing("Reference 6", refModel)
      }
      implicit val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.theModel), """[[Referring Model._instances -> _sort(Reference -> Prop to Sort) -> ""[[Name]]: [[Reference -> Prop to Sort]]""]]""") should
        equal ("\nReference 1a: \nReference 3: \nReference 6: \nReference 5: Alphabetical!\nReference 2: Check!\nReference 4: Floobity!\nReference 1: Kazam!")      
    }
    
    "handle numbers correctly" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, Core.QList, "List of Ints")
        val myThing = new SimpleTestThing("My Thing", numProp(12, 83, 0, 44, 92873, 6))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> List of Ints -> _sort -> _commas]]""") should equal ("0, 6, 12, 44, 83, 92873")
    }
  }
    
  // === _take ===
  "_take" should {
    class TSpace extends CommonSpace {
      val thingWithList = new SimpleTestThing("My Thing", listTagsProp("One", "Two", "Three", "Four", "Five", "Six"))
      val thingWithEmptyList = new SimpleTestThing("My Empty Thing", listTagsProp())
    }
    
    "work normally" in {
      implicit val s = new TSpace
      
      pql("""[[My Thing -> My List of Tags -> _take(3) -> _commas]]""") should
        equal ("[One](One), [Two](Two), [Three](Three)")
    }
    
    "produce an empty list if too far" in {
      implicit val s = new TSpace
      
      pql("""[[My Thing -> My List of Tags -> _drop(5) -> _take(3) -> _commas]]""") should
        equal ("[Six](Six)")
    }
    
    "work with an empty List" in {
      implicit val s = new TSpace
      
      pql("""[[My Empty Thing -> My List of Tags -> _take(2) -> _commas]]""") should
        equal ("")      
    }
  }
}