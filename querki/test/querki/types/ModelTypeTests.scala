package querki.types

import models.Thing.emptyProps

import querki.test._

class ModelTypeTests extends QuerkiTests with ModelTypeDefiner {
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
        propOfModelType(SimplePropertyBundle(
          numberProp(3),
          textProp("Text in Instance"))))
  }
      
  "A simple ModelType" should {
    "be defineable and usable" in {
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.thingWithComplex), """[[Complex Prop -> Number in Model]]""") should
        equal ("3")      
      processQText(thingAsContext[TSpace](space, _.thingWithComplex), """[[Complex Prop -> Text in Model]]""") should
        equal ("Text in Instance")
    }
    
    // TODO: this doesn't work yet, but should:
//    "be accessible with dot syntax" in {
//      val space = new TSpace
//
//      processQText(thingAsContext[TSpace](space, _.thingWithComplex), """[[Complex Prop.Number in Model]]""") should
//        equal ("3")      
//    }    
    
    "be renderable as a whole" in {
      val space = new TSpace

      processQText(thingAsContext[TSpace](space, _.thingWithComplex), """[[Complex Prop]]""") should
        equal("""
            |: Number in Model : 3
            |: Text in Model : Text in Instance""".stripReturns)
    }
  }
}
