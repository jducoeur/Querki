package querki.types

import models.{FormFieldInfo, IndexedOID, ThingState}
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
      val rebuiltThing = originalThing.copy(pf = () => newProps)
      val newState = state.copy(things = state.things + (rebuiltThing.id -> rebuiltThing))
      
      val rebuiltResult = pqls("""[[Top Level Thing -> Meta Property]]""", newState)
      
      // In the end, nothing should look different except for the one value:
      assert(rebuiltResult == originalResult.replace("200", "99"))
    }
  }
  
  "_foreachProperty" should {
    "work for a simple Model Value" in {
      class TSpace extends CommonSpace with ModelTypeDefiner {
        val prop1 = new TestProperty(Core.IntType, ExactlyOne, "First Prop")
        val prop2 = new TestProperty(Core.IntType, ExactlyOne, "Second Prop")
        val prop3 = new TestProperty(Core.IntType, ExactlyOne, "Third Prop")
        val prop4 = new TestProperty(Core.IntType, ExactlyOne, "Fourth Prop")
        val prop5 = new TestProperty(Core.IntType, ExactlyOne, "Fifth Prop")
        
        val bottomModel = new SimpleTestThing("Bottom Model", prop1(1), prop2(2), prop3(3), prop4(4), prop5(5))
        
        val modelType = new ModelType(toid, bottomModel.id, 
          Core.toProps(
            Core.setName("__ Bottom Model Type")
          ))
        registerType(modelType)
    
        val propOfModelType = new TestProperty(modelType, ExactlyOne, "Complex Prop")
        
        val complexThing = new SimpleTestThing("My Complex Thing", propOfModelType(SimplePropertyBundle(prop3(9))))
      }
      implicit val space = new TSpace
      
      println(pql("""[[My Complex Thing -> Complex Prop -> _foreachProperty(""Name: [[_prop -> Name]]; Value: [[_val]]"") -> _bulleted]]"""))
    }
  }
}
