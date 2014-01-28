package querki.types

import models.Thing.emptyProps

import querki.test._

class ModelTypeTests extends QuerkiTests with ModelTypeDefiner {
  "A simple ModelType" should {
    "be defineable and usable" in {
      class TSpace extends CommonSpace {
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number in Model")
        val textProp = new TestProperty(TextType, ExactlyOne, "Text in Model")
        
        val modelForType = new SimpleTestThing("Model for Type",
            numberProp(0),
            textProp(""))
        
        val modelType = new ModelType(toid, modelForType.id, 
            Core.toProps(
              Core.setName("My Model Type")
                ))
        registerType(modelType)
        
        val propOfModelType = new TestProperty(modelType, ExactlyOne, "Complex Prop")
        
        val thingWithComplex = new SimpleTestThing("My Complex Thing",
            propOfModelType(emptyProps +
              numberProp(3) +
              textProp("Text in Instance")))
      }
      val space = new TSpace
    }
  }
}