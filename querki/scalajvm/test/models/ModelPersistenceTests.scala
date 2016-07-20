package models

import Thing._

import querki.persistence._
import querki.test._

import ModelPersistence._

class ModelPersistenceCoreTests extends QuerkiTests with ModelPersistence {
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

class ModelPersistenceTests(env:PersistEnv) extends PersistTest(env) with ModelPersistence {
  implicit val state = env.cdSpace.state
  
  checkSerialization(dh(env.cdSpace.eurythmics))
  checkSerialization(dh(env.cdSpace.genres))
  
  val complexSpace = new querki.types.ComplexSpace()(ecology)
  checkSerialization(dh(complexSpace.metaType))
  
  // And here's the big test:
  checkSerialization(dh(complexSpace.state))
}
