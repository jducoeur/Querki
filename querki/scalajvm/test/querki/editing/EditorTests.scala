package querki.editing

import querki.test._

class EditorTests extends QuerkiTests {
  "_checkList" should {
    class TSpace extends CommonSpace {
      val fruit = new SimpleTestThing("Fruit")
      val apple = new TestThing("Apple", fruit)
      val banana = new TestThing("Banana", fruit)
      val blackberry = new TestThing("Blackberry", fruit)
      val cherry = new TestThing("Cherry", fruit)
      val raspberry = new TestThing("Raspberry", fruit)

      val fruits = new TestProperty(LinkType, QSet, "Fruits")
      val basket = new SimpleTestThing("Basket", fruits(banana, cherry))
      val emptyBasket = new SimpleTestThing("Empty Basket", fruits())
    }

    implicit class strTests(str: String) {
      def has(
        t: models.Thing,
        checked: Boolean
      ) = {
        val tid = t.id.toThingId.toString
        if (checked)
          str should include(s"""<input class="_checkOption" value="$tid" type="checkbox" checked="checked" />""")
        else
          str should include(s"""<input class="_checkOption" value="$tid" type="checkbox"></input>""")
      }

      def lacks(t: models.Thing) = {
        val tid = t.id.toThingId.toString
        (str should not).include(tid)
      }
    }

    "work normally with all instances" in {
      implicit val s = new TSpace
      import s._

      val res = pql("""[[Fruit._instances -> Fruits._checkList(on = Basket)]]""")
      res.has(apple, false)
      res.has(banana, true)
      res.has(blackberry, false)
      res.has(cherry, true)
      res.has(raspberry, false)
    }

    "work with selectedOnly" in {
      implicit val s = new TSpace
      import s._

      val res = pql("""[[Fruit._instances -> Fruits._checkList(on = Basket, selectedOnly = true)]]""")
      res.lacks(apple)
      res.has(banana, true)
      res.lacks(blackberry)
      res.has(cherry, true)
      res.lacks(raspberry)
    }

    // Regression test:
    "cope if nothing is selected" in {
      implicit val s = new TSpace
      import s._

      val res = pql("""[[Fruit._instances -> Fruits._checkList(on = Empty Basket, selectedOnly = true)]]""")
      res.lacks(apple)
      res.lacks(banana)
      res.lacks(blackberry)
      res.lacks(cherry)
      res.lacks(raspberry)
    }
  }

  "_edit" should {

    "let me edit a single Property" in {
      implicit val s = commonSpace
      println(pql("""[[My Instance -> My Optional Text._edit]]"""))
    }

    "enable me to edit a simple Thing" in {
      // Need to have editing rights to the Space:
      implicit val user = commonSpace.owner

      val trivialOID = commonSpace.trivialThing.id.toString
      val trivialThingId = commonSpace.trivialThing.id.toThingId

      // TODO: so far, this is just a trivial sanity-check of Display Name. It should become much more robust:
      // TODO: instead of this raw string check, we should be using XhtmlParser to parse the string, and examine the
      // structure:
      processQText(commonThingAsContext(_.trivialThing), """[[_edit]]""") should
        include(
          s"""<input data-thing="$trivialThingId" id="v-q-$trivialOID" data-collId=".e" data-propId="q" data-prop=".q" name="v-q-$trivialOID" class="_textEdit form-control propEditor" type="text" value="">"""
        )
    }
  }
}
