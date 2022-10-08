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

    // Test for Issue .3y286sw
    "give a good error in case of type mismatch" in {
      class TSpace extends CDSpace {
        val favoriteGenresProp = new TestProperty(TagType, QSet, "Favorite Genres", Links.LinkModelProp(genreModel))

        new SimpleTestThing(
          "More Favorites",
          favoriteArtistsProp(tmbg, blackmores),
          favoriteGenresProp("Rock", "Weird")
        )
      }
      implicit val s = new TSpace

      // TODO: this is mysteriously flaky, sometimes returning the Func.generalWrongType warning instead. Maddening:
      // fix this!
      pql("""[[More Favorites -> _concat(Favorite Artists, Favorite Genres)]]""") should
        equal(expectedWarning("Collections.concat.mismatchedTypes"))
    }

    // Test for QI.7w4g9hb:
    "cope with empty Lists" in {
      implicit val s = commonSpace

      pql("""[[_concat(< >, <""a"", ""b"">)]]""") should equal("\na\nb")
      pql("""[[_concat(<""a"", ""b"">, < >)]]""") should equal("\na\nb")
      pql("""[[_concat(<""a"">, < >, <""b"">)]]""") should equal("\na\nb")
    }

    "reject mismatched types" in {
      implicit val s = commonSpace

      pql("""[[_concat(<""a"", ""b"">, <1, 2>)]]""") should equal(expectedWarning("Collections.concat.mismatchedTypes"))
    }

    "cope with coercion" in {
      // This is adapted from a test in DateTimeTests, which is already checking coercion:
      import querki.time._
      lazy val Time = interface[querki.time.Time]

      class TSpace extends CommonSpace {
        val sortProp = new TestProperty(Basic.QLType, ExactlyOne, "Sort Function")

        val dateTimeProp = new TestProperty(Time.QDateTime, ExactlyOne, "DateTime Prop")
        val dateTimeModel = new SimpleTestThing("DateTime Model", sortProp("DateTime Prop"))
        val thing1 = new TestThing("Thing 1", dateTimeModel, dateTimeProp(new DateTime(2013, 3, 15, 10, 30)))
        val thing2 = new TestThing("Thing 2", dateTimeModel, dateTimeProp(new DateTime(2013, 4, 15, 10, 30)))

        val dateProp = new TestProperty(Time.QDate, ExactlyOne, "Date Prop")
        val dateModel = new SimpleTestThing("Date Model", sortProp("Date Prop"))
        val thing1a = new TestThing("Thing 1a", dateModel, dateProp(new DateTime(2016, 3, 15, 10, 30)))
        val thing2a = new TestThing("Thing 2a", dateModel, dateProp(new DateTime(2012, 4, 15, 10, 30)))
      }
      implicit val s = new TSpace

      pql("""[[_concat(Date Model._instances -> Date Prop, DateTime Model._instances -> DateTime Prop)]]""") should
        equal("\n03/15/2016\n04/15/2012\n03/15/2013\n04/15/2013")
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

      pql("""[[My Thing -> My Prop -> _contains(9)]]""") should equal("true")
      pql("""[[My Thing -> My Prop -> _contains(42)]]""") should equal("false")
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

      pql("""[[My Thing -> My Prop -> _contains(Thing 3)]]""") should equal("true")
      pql("""[[My Thing -> My Prop -> _contains(Thing 2)]]""") should equal("false")
    }

    // Test for QI.7w4ga0g:
    "work when checking against a collection" in {
      implicit val s = commonSpace

      // No flag, so matches if *any* are present:
      pql("""[[<1, 2, 3, 4, 5, 6> -> _contains(<12, 9, 4, 16>)]]""") should equal("true")
      // Fail if *none* are present:
      pql("""[[<1, 2, 3, 4, 5, 6> -> _contains(<12, 9, 72, 16>)]]""") should equal("false")
      // Flag, so requires that *all* be present:
      pql("""[[<1, 2, 3, 4, 5, 6> -> _contains(<3, 1, 5>, all=true)]]""") should equal("true")
      // Fail is any are missing:
      pql("""[[<1, 2, 3, 4, 5, 6> -> _contains(<3, 11, 5>, all=true)]]""") should equal("false")
    }
  }

  // === _contains ===
  "_isContainedIn" should {
    "work with simple numbers" in {
      class TSpace extends CommonSpace {
        val listProp = new TestProperty(Core.IntType, QList, "My Prop")
        val myThing = new SimpleTestThing("My Thing", listProp(5, 9, 92))
      }
      implicit val s = new TSpace

      pql("""[[9 -> _isContainedIn(My Thing -> My Prop)]]""") should equal("true")
      pql("""[[42 -> _isContainedIn(My Thing -> My Prop)]]""") should equal("false")
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

      pql("""[[Thing 3 -> _isContainedIn(My Thing -> My Prop)]]""") should equal("true")
      pql("""[[Thing 2 -> _isContainedIn(My Thing -> My Prop)]]""") should equal("false")
    }

    "work when checking against a collection" in {
      implicit val s = commonSpace

      // No flag, so matches if *any* are present:
      pql("""[[<12, 9, 4, 16> -> _isContainedIn(<1, 2, 3, 4, 5, 6>)]]""") should equal("true")
      // Fail if *none* are present:
      pql("""[[<12, 9, 72, 16> -> _isContainedIn(<1, 2, 3, 4, 5, 6>)]]""") should equal("false")
      // Flag, so requires that *all* be present:
      pql("""[[<3, 1, 5> -> _isContainedIn(<1, 2, 3, 4, 5, 6>, all=true)]]""") should equal("true")
      // Fail is any are missing:
      pql("""[[<3, 11, 5> -> _isContainedIn(<1, 2, 3, 4, 5, 6>, all=true)]]""") should equal("false")
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
        equal("[Four](Four), [Five](Five), [Six](Six)")
    }

    "produce an empty list if too far" in {
      implicit val s = new TSpace

      pql("""[[My Thing -> My List of Tags -> _drop(8) -> _commas]]""") should
        equal("")
    }

    "work with an empty List" in {
      implicit val s = new TSpace

      pql("""[[My Empty Thing -> My List of Tags -> _drop(2) -> _commas]]""") should
        equal("")
    }
  }

  // === _filter ===
  "_filter" should {
    "work with _equals" in {
      class TSpace extends CommonSpace {
        val linkTarget = new SimpleTestThing("Link Target")
        val pointer1 = new SimpleTestThing("Pointer 1", singleLinkProp(linkTarget))
        val pointer2 = new SimpleTestThing("Pointer 2", singleLinkProp(sandbox))
        val pointer3 = new SimpleTestThing("Pointer 3", singleLinkProp(linkTarget))
        val pointer4 = new SimpleTestThing("Pointer 4", singleLinkProp(trivialThing))
        val wrapper = new SimpleTestThing("Wrapper", listLinksProp(pointer1, pointer2, pointer3, pointer4))
      }
      implicit val s = new TSpace

      pql("""[[Wrapper -> My List of Links -> _filter(_equals(Single Link, Link Target))]]""") should
        equal(listOfLinkText(s.pointer1, s.pointer3))

      // Confirm that a single output comes out correctly:
      pql("""[[Wrapper -> My List of Links -> _filter(_equals(Single Link, Sandbox))]]""") should
        equal(listOfLinkText(s.pointer2))
    }
  }

  // === _fold ===
  "_fold" should {
    "work with an explicit accumulator" in {
      implicit val s = commonSpace

      pql("""[[<1, 2, 3, 4> -> _fold(0, $acc -> _plus($next))]]""") should equal("10")
    }

    "work with the context as the accumulator" in {
      implicit val s = commonSpace

      pql("""[[<1, 2, 3, 4> -> _fold(0, _plus($next))]]""") should equal("10")
    }

    "work with a different initial value" in {
      implicit val s = commonSpace

      pql("""[[<1, 2, 3, 4> -> _fold(10, $acc -> _plus($next))]]""") should equal("20")
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

        val myThing =
          new SimpleTestThing("My Thing", myQLProp("""""I got:[[$_context]]"""""), listProp(thing1, thing2, thing3))
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

  // === _nextInList and _prevInList ===
  "_nextInList and _prevInList" should {
    class TSpace extends CommonSpace {
      val fruit = new SimpleTestThing("Fruit")
      val apple = new TestThing("Apple", fruit)
      val pear = new TestThing("Pear", fruit)
      val banana = new TestThing("Banana", fruit)
      val blackberry = new TestThing("Blackberry", fruit)
      val kiwi = new TestThing("Kiwi", fruit)

      val numList = new TestProperty(Core.IntType, QList, "Num List")
      val withNums = new SimpleTestThing("With Nums", numList(2, 4, 8, 16, 32, 64))
    }

    "work in the middle of the list" in {
      implicit val s = new TSpace

      pql("""[[Blackberry -> _prevInList(Fruit._instances)]]""") should
        equal(linkText(s.banana))
      pql("""[[Blackberry -> _nextInList(Fruit._instances)]]""") should
        equal(linkText(s.kiwi))
    }

    "work at the beginning of the list" in {
      implicit val s = new TSpace

      pql("""[[Apple -> _prevInList(Fruit._instances)]]""") should
        equal("")
      pql("""[[Apple -> _nextInList(Fruit._instances)]]""") should
        equal(linkText(s.banana))
    }

    "work at the end of the list" in {
      implicit val s = new TSpace

      pql("""[[Pear -> _prevInList(Fruit._instances)]]""") should
        equal(linkText(s.kiwi))
      pql("""[[Pear -> _nextInList(Fruit._instances)]]""") should
        equal("")
    }

    "work with an empty list" in {
      implicit val s = new TSpace

      pql("""[[Pear -> _prevInList(Fruit._instances -> _drop(10))]]""") should
        equal("")
      pql("""[[Pear -> _nextInList(Fruit._instances -> _drop(10))]]""") should
        equal("")
    }

    "work with numbers" in {
      implicit val s = new TSpace

      pql("""[[8 -> _prevInList(With Nums -> Num List)]]""") should
        equal("4")
      pql("""[[8 -> _nextInList(With Nums -> Num List)]]""") should
        equal("16")
    }

    "work if the element isn't in the list" in {
      implicit val s = new TSpace

      pql("""[[7 -> _prevInList(With Nums -> Num List)]]""") should
        equal("")
      pql("""[[7 -> _nextInList(With Nums -> Num List)]]""") should
        equal("")
    }
  }

  // === _reverse ===
  "_reverse" should {
    "work correctly with an ordinary list" in {
      implicit val space = new CDSpace

      pql("""[[My Favorites -> Favorite Artists -> Artists._refs -> _sort -> _reverse]]""") should
        equal(listOfLinkText(
          space.shadowOfTheMoon,
          space.ghostOfARose,
          space.flood,
          space.firesAtMight,
          space.factoryShowroom
        ))
    }

    "work with an empty input" in {
      implicit val space = new CDSpace

      pql("""[[Whitney Houston -> Artists._refs -> _sort -> _reverse]]""") should
        equal(listOfLinkText())
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
        equal(listOfLinkText(pointer2, pointer1, linkTarget))
    }

    "sort by name with _desc" in {
      val space = new testSpace
      import space._

      processQText(
        thingAsContext[testSpace](space, _.wrapper),
        """[[My List of Links -> _sort(_desc(Link Name))]]"""
      ) should
        equal(listOfLinkText(linkTarget, pointer1, pointer2))
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

      processQText(
        thingAsContext[TSpace](space, _.theModel),
        """[[Sorting Model._instances -> _sort(Prop to Sort) -> ""[[Link Name]]: [[Prop to Sort]]""]]"""
      ) should
        equal("\nThing 5: Alphabetical!\nThing 2: Check!\nThing 4: Floobity!\nThing 1: Kazam!\nThing 3: Wild!")
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

      processQText(
        thingAsContext[TSpace](space, _.theModel),
        """[[Referring Model._instances -> _sort(Reference -> Prop to Sort, ""Unknown"") -> ""[[Link Name]]: [[Reference -> Prop to Sort]]""]]"""
      ) should
        equal(
          "\nReference 5: Alphabetical!\nReference 2: Check!\nReference 4: Floobity!\nReference 1: Kazam!\nReference 3: Wild!"
        )
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

      pql(
        """[[Sorting Model._instances -> _sort(Prop to Sort, Number Prop, Another Prop) -> ""[[Prop to Sort]], [[Number Prop]], [[Another Prop]]""]]"""
      ) should
        equal("""
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

      processQText(
        thingAsContext[TSpace](space, _.theModel),
        """[[Referring Model._instances -> _sort(Reference -> Prop to Sort) -> ""[[Link Name]]: [[Reference -> Prop to Sort]]""]]"""
      ) should
        equal(
          "\nReference 1a: \nReference 3: \nReference 6: \nReference 5: Alphabetical!\nReference 2: Check!\nReference 4: Floobity!\nReference 1: Kazam!"
        )
    }

    "handle numbers correctly" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, Core.QList, "List of Ints")
        val myThing = new SimpleTestThing("My Thing", numProp(12, 83, 0, 44, 92873, 6))
      }
      implicit val s = new TSpace

      pql("""[[My Thing -> List of Ints -> _sort -> _commas]]""") should equal("0, 6, 12, 44, 83, 92873")
    }

    "sort display names case-insensitively" in {
      class TSpace extends CommonSpace {
        val ordinals = new SimpleTestThing("Ordinals")
        val first = new TestThing("First", ordinals)
        val second = new TestThing("Second", ordinals)
        val third = new TestThing("Third", ordinals)
        // Note the lower-case -- that's what we are testing:
        val fourth = new TestThing("fourth", ordinals)
      }
      implicit val s = new TSpace

      pql("""[[Ordinals._instances]]""") should
        equal(listOfLinkText(s.first, s.fourth, s.second, s.third))
      pql("""[[Ordinals._instances -> _sort]]""") should
        equal(listOfLinkText(s.first, s.fourth, s.second, s.third))
    }

    "handle Tags correctly" in {
      class TSpace extends CommonSpace {
        // This test data is drawn from Issue .3y28aek -- this is essentially a regression test for that:
        val withTags =
          new SimpleTestThing("With Tags", listTagsProp("Underwater Branch", "Canoe", "Lug the Undulous Burden", "elk"))
      }
      implicit val s = new TSpace

      // Test that it sorts case-insensitively:
      pql("""[[With Tags -> My List of Tags -> _sort]]""") should
        equal(listOfTags("Canoe", "elk", "Lug the Undulous Burden", "Underwater Branch"))
    }

    "sort text fields case-insensitively" in {
      class MySpace extends CommonSpace {
        val textModel = new SimpleTestThing("Text Model", singleTextProp())

        val first = new TestThing("First", textModel, singleTextProp("Text in Instance"))
        val second = new TestThing("Second", textModel, singleTextProp("Some other text"))
        val third = new TestThing("Third", textModel, singleTextProp("More text?"))
        val fourth = new TestThing("Fourth", textModel, singleTextProp("Text in Instance"))
        val fifth = new TestThing("Fifth", textModel, singleTextProp("blah"))
        val sixth = new TestThing("Sixth", textModel, singleTextProp("text in instance"))
      }
      implicit val s = new MySpace

      pql("""[[Text Model._instances -> _sort(Single Text)]]""") should
        equal(listOfLinkText(s.fifth, s.third, s.second, s.first, s.fourth, s.sixth))
    }

    "handle sorting on fields of complex types" in {
      import querki.types.{ComplexSpace, SimplePropertyBundle}

      class ComplexSpaceWithList extends CommonSpace {
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number in Model")
        val textProp = new TestProperty(TextType, ExactlyOne, "Text in Model")
        val modelForType = new SimpleTestThing("Model for Type", numberProp(0), textProp(""))
        val modelType = new ModelType(
          toid,
          modelForType.id,
          Core.toProps(
            Core.setName("My Model Type")
          )
        )
        registerType(modelType)
        val listOfModelType = new TestProperty(modelType, QList, "Complex List")

        val thingWithModelList = new SimpleTestThing(
          "Thing With Model List",
          listOfModelType(
            SimplePropertyBundle(
              Core.NameProp("First"),
              numberProp(3),
              textProp("Text in Instance")
            ),
            SimplePropertyBundle(
              Core.NameProp("Second"),
              numberProp(12),
              textProp("Some other text")
            ),
            SimplePropertyBundle(
              Core.NameProp("Third"),
              numberProp(2),
              textProp("More text?")
            ),
            SimplePropertyBundle(
              Core.NameProp("Fourth"),
              numberProp(839),
              textProp("Text in Instance")
            ),
            SimplePropertyBundle(
              Core.NameProp("Fifth"),
              numberProp(3),
              textProp("blah")
            ),
            SimplePropertyBundle(
              Core.NameProp("Sixth"),
              numberProp(-84),
              textProp("text in instance")
            )
          )
        )
      }
      implicit val s = new ComplexSpaceWithList

      // Sort by number:
      pql("""[[Thing With Model List -> Complex List -> _sort(Number In Model) -> Link Name -> _commas]]""") should
        equal("""Sixth, Third, First, Fifth, Second, Fourth""")
      // Sort by text:
      pql("""[[Thing With Model List -> Complex List -> _sort(Text In Model) -> Link Name -> _commas]]""") should
        equal("""Fifth, Third, Second, First, Fourth, Sixth""")
      // Sort by both:
      pql(
        """[[Thing With Model List -> Complex List -> _sort(Number In Model, Text In Model) -> Link Name -> _commas]]"""
      ) should
        equal("""Sixth, Third, Fifth, First, Second, Fourth""")
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
        equal("[One](One), [Two](Two), [Three](Three)")
    }

    "produce an empty list if too far" in {
      implicit val s = new TSpace

      pql("""[[My Thing -> My List of Tags -> _drop(5) -> _take(3) -> _commas]]""") should
        equal("[Six](Six)")
    }

    "work with an empty List" in {
      implicit val s = new TSpace

      pql("""[[My Empty Thing -> My List of Tags -> _take(2) -> _commas]]""") should
        equal("")
    }
  }

  // === _toSet ===
  "_toSet" should {
    "work with a plain QList" in {
      implicit val s = new CDSpace

      pql("""[[<Blackmores Night, Weird Al, Blackmores Night> -> _toSet]]""") should equal(listOfLinkText(
        s.blackmores,
        s.weirdAl
      ))
    }

    // Regression test for QI.7w4ghfr: if you have duplicate inputs to _withValueIn(), and a complex matrix
    // of the relationships between those and the Model instances, you can wind up with duplicates in the output.
    // Make sure we can handle that. This was the motivating use case for _toSet.
    "work with a more complex output" in {
      implicit val s = new CDSpace

      pql("""[[<Blackmores Night, Weird Al, Blackmores Night> -> Album._withValueIn(Artists) -> _toSet]]""") should
        equal(listOfLinkText(
          s.firesAtMight,
          s.ghostOfARose,
          s.mandatoryFun,
          s.runningWithScissors,
          s.shadowOfTheMoon
        ))
    }
  }
}
