package querki.types

import models.Thing.emptyProps

import querki.ecology._
import querki.test._

class ComplexSpace(implicit ec:Ecology) extends CommonSpace with ModelTypeDefiner {
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
  
  val metaModel = new SimpleTestThing("Meta Model",
      propOfModelType(SimplePropertyBundle(
        numberProp(42),
        textProp("Text from MetaModel"))))
  val metaType = new ModelType(toid, metaModel.id,
      Core.toProps(
        Core.setName("Meta Type")))
  registerType(metaType)
  val metaProp = new TestProperty(metaType, QList, "Meta Property")
  
  val metaThing = new SimpleTestThing("Top level Thing",
      metaProp(
        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(100), textProp("Top Text 1")))),
        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(200), textProp("Top Text 2"))))))
}
      
class ModelTypeTests extends QuerkiTests {
  "A simple ModelType" should {
    "be defineable and usable" in {
      val space = new ComplexSpace
      
      processQText(thingAsContext[ComplexSpace](space, _.thingWithComplex), """[[Complex Prop -> Number in Model]]""") should
        equal ("3")      
      processQText(thingAsContext[ComplexSpace](space, _.thingWithComplex), """[[Complex Prop -> Text in Model]]""") should
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
      val space = new ComplexSpace

      processQText(thingAsContext[ComplexSpace](space, _.thingWithComplex), """[[Complex Prop]]""") should
        equal("""
            |: Number in Model : 3
            |: Text in Model : Text in Instance""".stripReturns)
    }
  }
  
  "A nested Model Type" should {
    "be defineable and usable" in {
      implicit val space = new ComplexSpace
      
      pql("""[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Text in Model]]""") should
        equal("Top Text 1")
    }
  }
}
