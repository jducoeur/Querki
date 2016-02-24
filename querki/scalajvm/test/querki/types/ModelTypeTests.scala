package querki.types

import models.{FormFieldInfo, IndexedOID, ThingState}
import models.Thing.emptyProps

import querki.ecology._
import querki.test._

class ComplexSpace(implicit ec:Ecology) extends CommonSpace with ModelTypeDefiner {
  lazy val Editor = interface[querki.editing.Editor] 
  
  val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number in Model")
  val textProp = new TestProperty(TextType, ExactlyOne, "Text in Model")
  val referencingProp = new TestProperty(TextType, ExactlyOne, "Referencing")
    
  val modelForType = new SimpleTestThing("Model for Type",
      numberProp(0),
      textProp(""),
      referencingProp("[[Top Level Property]]"),
      Editor.InstanceEditViewProp("Reference from subModel: [[Top Level Property]]"))
    
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
  
  val topLevelProp = new TestProperty(TextType, ExactlyOne, "Top Level Property")
  
  val metaThing = new SimpleTestThing("Top level Thing", topLevelProp("From the Top"),
      metaProp(
        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(100), textProp("Top Text 1")))),
        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(200), textProp("Top Text 2"))))))
  
  // Now, here is the complex one. We're building a recursive tree, so we need to define the model, then the
  // type, then the property, then redefine the model again.
  //
  // Note that this is a proof of concept for a binary tree. If we can do this, the sky's the limit.
  val nodeId = new TestProperty(TextType, Optional, "Node Id")
  val nodeModelId = toid()
  val nodeType = new ModelType(toid, nodeModelId,
      Core.toProps(
        Core.setName("Node Type")))
  registerType(nodeType)
  val leftProp = new TestProperty(nodeType, Optional, "Left")
  val rightProp = new TestProperty(nodeType, Optional, "Right")
  val nodeModel = new TestThing(nodeModelId, "Tree Node Model", Core.IsModelProp(true), leftProp(), rightProp(), nodeId())
  
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
            |<dt>Number in Model</dt><dd>3</dd>
            |<dt>Text in Model</dt><dd>Text in Instance</dd>""".stripReturns)
    }
    
    "be able to generate an Edit control" in {
      val Editor = interface[querki.editing.Editor]
      implicit val space = new ComplexSpace
      implicit val user = space.owner
      
      val result = pql("""[[Top Level Thing -> _edit]]""")
    }
  }
  
  "A nested Model Type" should {
    "be defineable and usable" in {
      implicit val space = new ComplexSpace
      
      pql("""[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Text in Model]]""") should
        equal("Top Text 1")
    }
    
    "be able to access properties from its Thing" in {
      implicit val s = new ComplexSpace
      
      // This is horribly messy, but Referencing then refers back to a Property defined all the way up
      // at the top:
      pql("""[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Referencing]]""") should equal ("From the Top")
    }
    
    "be able to access its Thing from _edit" in {
      implicit val s = new ComplexSpace
      
      pql("[[Top Level Thing -> Meta Property -> _first -> Complex Prop._edit]]") should include ("Reference from subModel: From the Top")
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
    "work for a reasonably nested value with an index" in {
      implicit val space = new ComplexSpace
      val state = space.state
      val Types = interface[querki.types.Types]

      // In other words, we have received a new value for numberProp...
      val detailChange = FormFieldInfo(space.numberProp, Some(space.numberProp(99)._2), false, true)
      
      val originalResult = pql("""[[Top Level Thing -> Meta Property]]""")
      
      // ... which is nested inside of metaProp[1]-propOfModelType. The original of this is 200.
      val resultOpt = Types.rebuildBundle(
          Some(space.metaThing), 
          IndexedOID(space.metaProp.id, Some(1)) :: space.propOfModelType.iid :: Nil, 
          detailChange)(state)
      val result = resultOpt.get
      
      // Given the new value, rebuilt the SpaceState. (In theory, we should be doing this through
      // Space.scala, but we're not ready for that yet.)
      val originalThing:ThingState = space.metaThing
      val newProps = space.metaThing.props + (result.propId -> result.value.get)
      val rebuiltThing = originalThing.copy(pf = newProps)
      val newState = state.copy(things = state.things + (rebuiltThing.id -> rebuiltThing))
      
      val rebuiltResult = pqls("""[[Top Level Thing -> Meta Property]]""", newState)
      
      // In the end, nothing should look different except for the one value:
      assert(rebuiltResult == originalResult.replace("200", "99"))
    }
  }
}
