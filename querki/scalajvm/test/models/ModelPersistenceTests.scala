package models

import Thing._

import querki.test._

import ModelPersistence._

class ModelPersistenceTests extends QuerkiTests with ModelPersistence {
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
