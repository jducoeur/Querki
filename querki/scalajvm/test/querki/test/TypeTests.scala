package querki.test

import querki.globals._

class TypeTests extends QuerkiTests {
  lazy val TextType = Core.TextType

  "Set of Text" should {
    "render appropriately" in {
      class TSpace extends CommonSpace {
        val textSetProp = new TestProperty(TextType, QSet, "My Text Set")

        val myThing = new SimpleTestThing(
          "My Thing",
          textSetProp("First Value", "Second Value, with *emphasis*", "Third Value", "Fourth Value")
        )
      }
      val space = new TSpace
      implicit val s = space.state

//      for (
//        propAndVal <- space.myThing.getPropOpt(space.textSetProp);
//        qv = propAndVal.v;
//        context = thingAsContext[TSpace](space, (_.myThing));
//        wikified = awaitIntentionally(qv.wikify(context, None))
//      )
//        println(wikified.display)

      processQText(thingAsContext[TSpace](space, (_.myThing)), "[[My Text Set -> _bulleted]]") should
        equal("""
            |<ul>
			|<li class="_bullet">
			|First Value
			|</li>
			|<li class="_bullet">
			|Second Value, with *emphasis*
			|</li>
			|<li class="_bullet">
			|Third Value
			|</li>
			|<li class="_bullet">
			|Fourth Value
			|</li>
			|</ul>""".stripReturns)
    }
  }

  "List of Text" should {
    "render appropriately" in {
      class TSpace extends CommonSpace {
        val textListProp = new TestProperty(TextType, QList, "My Text List")

        val myThing =
          new SimpleTestThing("My Thing", textListProp("First Value", "Second Value", "Third Value", "Fourth Value"))
      }
      val space = new TSpace
      implicit val s = space.state

      processQText(thingAsContext[TSpace](space, (_.myThing)), "[[My Text List]]") should
        equal("\nFirst Value\nSecond Value\nThird Value\nFourth Value")
    }
  }
}
