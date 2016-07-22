package models

import ai.x.diff.DiffShow
import ai.x.diff.conversions._

import Thing._

import querki.globals._
import querki.persistence._
import querki.test._

import ModelPersistence._

class ModelPersistenceCoreTests extends QuerkiTests with ModelPersistence with querki.types.ModelTypeDefiner {
  "PropMap" should {
    "roundtrip properly" in {
      val s = new CDSpace
      implicit val state = s.state
      
      val original = s.blackmores.props
      val dh:DHPropMap = original
      val copy:PropMap = dh
      
      def checkProp(prop:AnyProp) = {
        assert(prop.from(copy).matches(prop.from(original)))
      }
      
      checkProp(Core.NameProp)
      checkProp(s.genres)
    }
  }
}

class ModelPersistenceTests(env:PersistEnv) extends PersistTest(env) 
  with ModelPersistence with querki.types.ModelTypeDefiner with ModelDiff 
{
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
//  if (rehydrated != complexState) {
//    // Humph -- mismatched.
//    QLog.spew("Rehydrated State doesn't match original!")
//    println(DiffShow.diff(complexState, rehydrated).string)
//    throw new Exception(s"Rehydrated Space didn't match the original!")
//  }
}
