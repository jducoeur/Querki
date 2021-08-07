package models

import ai.x.diff.DiffShow
import ai.x.diff.conversions._

import querki.globals._
import querki.persistence._
import querki.test._

import ModelPersistence._

class ModelPersistenceCoreTests extends QuerkiTests with ModelPersistence with querki.types.ModelTypeDefiner {
  "PropMap" should {
    "roundtrip properly" in {
      val s = new CDSpace
      implicit val state = s.state

      val original = s.blackmores.props + (s.singleLinkProp.id -> DataModelAccess.getDeletedValue(s.singleLinkProp))
      val dh: DHPropMap = original
      val copy: PropMap = dh

      def checkProp(prop: AnyProp) = {
        assert(prop.from(copy).matches(prop.from(original)))
      }

      checkProp(Core.NameProp)
      checkProp(s.genres)
      assert(s.singleLinkProp.from(copy).isDeleted)
    }
  }
}

class ModelPersistenceTests(env: PersistEnv)
  extends PersistTest(env)
     with ModelPersistence
     with querki.types.ModelTypeDefiner
     with ModelDiff {

  def runTests() = {
    implicit val state = env.cdSpace.state

    checkSerialization(dh(env.cdSpace.eurythmics))
    checkSerialization(dh(env.cdSpace.genres))

    val complexSpace = new querki.types.ComplexSpace()(ecology)
    checkSerialization(dh(complexSpace.metaType))

    // Roundtrip a dehydrated SpaceState. Ignore the cache, which isn't expected to be right:
    val complexState = complexSpace.state.copy(cache = Map.empty)
    val spacedh = dh(complexState)
    val spaceCopy = checkSerialization(spacedh)
    // And rehydrate it:
    val rehydrated = rehydrate(spaceCopy)
    testRehydratedComplexState(rehydrated)
  }

  def testRehydratedComplexState(state: SpaceState) = {
    implicit val s = new DynamicSpace(state)(ecology)

    // These tests are intentionally lifted from ModelTypeTests. Arguably, we should
    // refactor them out:
    env.pqlEquals("""[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Text in Model]]""", "Top Text 1")
    env.pqlEquals("""[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Referencing]]""", "From the Top")
    env.pqlEquals("""[[My Tree -> Left -> Right -> Node Id]]""", "3")
  }

  runTests()
}
