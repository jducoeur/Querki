package querki.types

import querki.globals._
import querki.spaces._
import querki.test._

/**
 * This is the same as ComplexSpace, but built dynamically instead of statically, so it
 * is mutable.
 */
class ComplexCoreSpace(implicit e:Ecology) extends CommonCoreSpace() {
  lazy val Editor = interface[querki.editing.Editor] 
  
  val numberProp = addProperty(Core.IntType, ExactlyOne, "Number in Model")
  val textProp = addProperty(TextType, ExactlyOne, "Text in Model")
  val referencingProp = addProperty(TextType, ExactlyOne, "Referencing")
    
  val modelForType = addSimpleThing("Model for Type",
      numberProp(0),
      textProp(""),
      referencingProp("[[Top Level Property]]"),
      Editor.InstanceEditViewProp("Reference from subModel: [[Top Level Property]]"))
  val modelType = addType("My Model Type", modelForType.id)
  val propOfModelType = addProperty(modelType, ExactlyOne, "Complex Prop")
  
  val thingWithComplex = addSimpleThing("My Complex Thing",
      propOfModelType(SimplePropertyBundle(
        numberProp(3),
        textProp("Text in Instance"))))
  
  val metaModel = addSimpleThing("Meta Model",
      propOfModelType(SimplePropertyBundle(
        numberProp(42),
        textProp("Text from MetaModel"))))
  val metaType = addType("Meta Type", metaModel.id)
  val metaProp = addProperty(metaType, QList, "Meta Property")
  
  val topLevelProp = addProperty(TextType, ExactlyOne, "Top Level Property")
  
  val metaThing = addSimpleThing("Top level Thing", topLevelProp("From the Top"),
      metaProp(
        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(100), textProp("Top Text 1")))),
        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(200), textProp("Top Text 2"))))))
  
  // Now, here is the complex one. We're building a recursive tree, so we need to define the model, then the
  // type, then the property, then redefine the model again. Unlike the original ModelTypeTests, we can do this
  // properly here.
  //
  // Note that this is a proof of concept for a binary tree. If we can do this, the sky's the limit.
  val nodeId = addProperty(TextType, Optional, "Node Id")
  val nodeModelId = addSimpleModel("Tree Node Model")
  val nodeType = addType("Node Type", nodeModelId)
  val leftProp = addProperty(nodeType, Optional, "Left")
  val rightProp = addProperty(nodeType, Optional, "Right")
  changeThing(nodeModelId, leftProp(), rightProp(), nodeId())
  
  val myTree = addThing("My Tree", nodeModelId,
      leftProp(SimplePropertyBundle(
        leftProp(SimplePropertyBundle(nodeId("1"))),
        rightProp(SimplePropertyBundle(nodeId("3"))),
        nodeId("2"))),
      rightProp(SimplePropertyBundle(
        rightProp(SimplePropertyBundle(nodeId("6"))),
        nodeId("5"))),
      nodeId("4"))  
}

/**
 * This is very similar to ModelTypeTests, but based around the newer SpaceCore machinery.
 */
class ModelTypeCoreTests extends ModelTypeTestBase {
  def makeSpace() = new ComplexCoreSpace
}
