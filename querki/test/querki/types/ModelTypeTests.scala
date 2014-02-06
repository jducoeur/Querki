package querki.types

import models.{FormFieldInfo, ThingState}
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
  
  // Now, here is the complex one. We're building a recursive tree, so we need to define the model, then the
  // type, then the property, then redefine the model again.
  //
  // Note that this is a proof of concept for a binary tree. If we can do this, the sky's the limit.
  val nodeId = new TestProperty(TextType, Optional, "Node Id")
  val initialNodeModel = new SimpleTestThing("Tree Node Model")
  val nodeType = new ModelType(toid, initialNodeModel.id,
      Core.toProps(
        Core.setName("Node Type")))
  registerType(nodeType)
  val leftProp = new TestProperty(nodeType, Optional, "Left")
  val rightProp = new TestProperty(nodeType, Optional, "Right")
  val nodeModel = new TestThing(initialNodeModel.id, "Tree Node Model", leftProp(), rightProp(), nodeId())
  
  val myTree = new TestThing("My Tree", nodeModel,
      leftProp(SimplePropertyBundle(
        leftProp(SimplePropertyBundle(nodeId("1"))),
        rightProp(SimplePropertyBundle(nodeId("3"))),
        nodeId("2"))),
      rightProp(SimplePropertyBundle(
        rightProp(SimplePropertyBundle(nodeId("6"))),
        nodeId("5"))),
      nodeId("4"))
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
    
    "be able to generate an Edit control" in {
      val Editor = interface[querki.editing.Editor]
      implicit val space = new ComplexSpace
      implicit val user = space.owner
      
      val result = pql("""[[Top Level Thing -> _edit]]""")
      println("----> " + result)
    }
  }
  
  "A nested Model Type" should {
    "be defineable and usable" in {
      implicit val space = new ComplexSpace
      
      pql("""[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Text in Model]]""") should
        equal("Top Text 1")
    }
  }
  
  "A recursive Model Type" should {
    "be defineable and usable" in {
      implicit val space = new ComplexSpace
      
      pql("""[[My Tree -> Left -> Right -> Node Id]]""") should
        equal("3")
    }
  }
  
  "rebuildBundle" should {
    "work for a reasonably nested value" in {
      implicit val space = new ComplexSpace
      val state = space.state
      val Types = interface[querki.types.Types]
      
      val detailChange = FormFieldInfo(space.numberProp, Some(space.numberProp(99)._2), false, true)
      
      println("Original: " + pql("""[[Top Level Thing -> Meta Property]]"""))
      println("MetaProp: " + space.metaProp.id + "; ComplexProp: " + space.propOfModelType.id)
      println("Actual Change " + detailChange)
      
      val resultOpt = Types.rebuildBundle(
          Some(space.metaThing), 
          space.metaProp.id :: space.propOfModelType.id :: Nil, 
          detailChange)(state)
      println("Result: " + resultOpt)
      val result = resultOpt.get
      
      val originalThing:ThingState = space.metaThing
      val newProps = space.metaThing.props + (result.propId -> result.value.get)
      val rebuiltThing = originalThing.copy(pf = () => newProps)
      val newState = state.copy(things = state.things + (rebuiltThing.id -> rebuiltThing))
      
      println("Rebuilt: " + pqls("""[[Top Level Thing -> Meta Property]]""", newState))
    }
  }
}
