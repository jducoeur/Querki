package querki.spaces

import querki.test._

import models._

import querki.ecology._
import querki.time.DateTime
import querki.types.{ComplexSpace, ModelTypeDefiner, SimplePropertyBundle}
import querki.values.{QValue, SpaceState}

/**
 * A stub for the "database" in Space saving/loading. Plays at least somewhat fair: truly serializes and
 * deserializes the Things.
 */
class SpaceLoadTestDB(val ecology: Ecology) extends EcologyMember with ThingStreamLoader {

  case class StoredThing(
    id: Long,
    kind: Int,
    model: Long,
    modified: Long,
    props: String
  )

  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]

  var db: Map[Long, StoredThing] = Map.empty

  def store(t: Thing)(implicit state: SpaceState) = {
    db = db + (t.id.raw -> StoredThing(
      t.id.raw,
      t.kind,
      t.model.raw,
      t.modTime.getMillis,
      SpacePersistence.serializeProps(t.props, state)
    ))
  }

  def storeSpace(implicit state: SpaceState) = {
    store(state)
    state.types.values.foreach(store(_))
    state.spaceProps.values.foreach(store(_))
    state.things.values.foreach(store(_))
  }

  def getThingList[T <: Thing](kind: Int)(state: SpaceState)(builder: (OID, OID, PropMap, DateTime) => T): List[T] = {
    val stores = db.values.filter(_.kind == kind).toList
    stores.map(store =>
      builder(
        OID(store.id),
        OID(store.model),
        SpacePersistence.deserializeProps(store.props, state),
        new DateTime(store.modified)
      )
    )
  }
}

/**
 * Stub for testing the SpaceLoader.
 */
class SpaceLoadTester(
  val state: SpaceState,
  val ecology: Ecology
) extends SpaceLoader
     with EcologyMember
     with ModelTypeDefiner {

  // ======================
  // Values expected by SpaceLoader
  //
  lazy val Core = interface[querki.core.Core]
  lazy val SystemInterface = interface[querki.system.System]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  lazy val Types = interface[querki.types.Types]

  def id: OID = state.id
  def name: String = state.name
  def owner: OID = state.owner

  // =======================

}

class SpaceLoadTests extends QuerkiTests {

  def saveLoad(space: CommonSpace): SpaceState = {
    val userTesting = interface[UserTesting]
    val db = new SpaceLoadTestDB(ecology)
    val loader = new SpaceLoadTester(commonState, ecology)

    userTesting.prepSpace(space)
    db.storeSpace(space.state)
    loader.doLoad(db, Seq.empty)
  }

  "SpacePersister" should {
    "successfully save and load the CommonSpace" in {
      val loadedState = saveLoad(commonSpace)

      // Just do a simple smoketest based on the contents of CommonSpace. This demonstrates that both
      // the property and instance have loaded:
      processQText(loadedContext(loadedState, commonSpace.instance.id), """[[My Optional Text]]""") should
        equal("""Hello world""")
    }

    "successfully save and load if the Space uses its own Property" in {
      class TestSpace extends CommonSpace {
        override def otherSpaceProps: Seq[(OID, QValue)] = Seq(optTextProp("I'm a Space!"))
      }
      val space = new TestSpace

      val loadedState = saveLoad(space)

      // Just do a simple smoketest based on the contents of CommonSpace. This demonstrates that both
      // the property and instance have loaded:
      processQText(loadedContext(loadedState, space.state.id), """[[My Optional Text]]""") should
        equal("""I'm a Space!""")
    }

    "successfully save and load a Space with Model Types" in {
      val space = new ComplexSpace

      val loadedState = saveLoad(space)

      processQText(
        loadedContext(loadedState, space.thingWithComplex.id),
        """[[Complex Prop -> Text in Model]]"""
      ) should
        equal("Text in Instance")

      processQText(
        loadedContext(loadedState, space.thingWithComplex.id),
        """[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Text in Model]]"""
      ) should
        equal("Top Text 1")

      processQText(
        loadedContext(loadedState, space.thingWithComplex.id),
        """[[My Tree -> Left -> Right -> Node Id]]"""
      ) should
        equal("3")
    }

    // Regression test for QI.9v5kc1r:
    "successfully save and load a Space with Model Types that contain Lists and Sets" in {
      class TSpace extends CommonSpace {
        val listProp = new TestProperty(Core.IntType, QList, "List of Ints")
        val tagSetProp = new TestProperty(Core.TagType, QSet, "Set of Tags")

        val innerModel = new SimpleTestThing("Inner", listProp(), tagSetProp())
        val innerType = new ModelType(
          toid,
          innerModel.id,
          Core.toProps(
            Core.setName("Inner Type")
          )
        )
        registerType(innerType)
        val listOfInners = new TestProperty(innerType, QList, "List of Inners")

        val outerModel = new SimpleTestThing("Outer Model", listOfInners())
        val outerWithOne = new TestThing(
          "Outer with One Inner",
          outerModel,
          listOfInners(SimplePropertyBundle(
            listProp(0, 1, 2, 3),
            tagSetProp("Fear", "Can Affect Adjacent", "Something Else")
          ))
        )

        val outerType = new ModelType(
          toid,
          outerModel.id,
          Core.toProps(
            Core.setName("Outer Type")
          )
        )
        registerType(outerType)
        val listOfOuters = new TestProperty(outerType, QList, "List of Outers")

        val outererModel = new SimpleTestThing("Outerer Model", listOfOuters())
        val outerer = new TestThing(
          "Outerer",
          outererModel,
          listOfOuters(
            SimplePropertyBundle(
              listOfInners(
                SimplePropertyBundle(
                  listProp(1, 2, 3),
                  tagSetProp("hello", "world")
                ),
                SimplePropertyBundle(
                  listProp(4, 5, 6),
                  tagSetProp("hi", "there")
                )
              )
            ),
            SimplePropertyBundle(
              listOfInners(
                SimplePropertyBundle(
                  listProp(7, 8, 9),
                  tagSetProp("Larry", "Moe", "Curly")
                ),
                SimplePropertyBundle(
                  listProp(21, 42, 999),
                  tagSetProp("me", "myself", "I")
                )
              )
            )
          )
        )
      }
      val space = new TSpace

      val loadedState = saveLoad(space)

      // The straightforward test:
      processQText(loadedContext(loadedState, space.outerWithOne.id), """[[List of Inners -> List of Ints]]""") should
        equal("\n0\n1\n2\n3")
      processQText(loadedContext(loadedState, space.outerWithOne.id), """[[List of Inners -> Set of Tags]]""") should
        equal(listOfTags("Fear", "Can Affect Adjacent", "Something Else"))
      // And now, the acid test:
      processQText(
        loadedContext(loadedState, space.outerer.id),
        """[[List of Outers -> _drop(1) -> List of Inners -> _first -> Set of Tags]]"""
      ) should
        equal(listOfTags("Larry", "Moe", "Curly"))
    }
  }
}
