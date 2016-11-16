package querki.apps

import models.AsName
import querki.globals._
import querki.spaces.SpaceCoreSpaceBase
import querki.test._
import querki.types.{ModelTypeBase, ModelTypeDefiner, SimplePropertyBundle}

class SpaceToExtract(implicit e:Ecology) extends AppableSpace {
  val textProp = addProperty(Core.TextType, Core.Optional, "Text Property")
  val numProp = addProperty(Core.IntType, Core.ExactlyOne, "Int Property")
  
  val modelForType = addSimpleModel("Model for Type", textProp("Type Model Default"), numProp(12))
  val typeModel = addType("Type Model", modelForType)
  val propOfModelType = addProperty(typeModel, Core.QList, "Complex Prop")
  
  val model1 = addSimpleModel("Model 1", textProp("Default Text"))
  val model2 = addSimpleModel("Model 2")
  val model3 = addSimpleModel("Model 3",
    propOfModelType(SimplePropertyBundle(
      textProp("Model 3 Default"),
      numProp(17)
    )))
  
  val instance11 = addThing("Instance 1 1", model1, textProp("Hello from Instance 1 1!"))
  val instance12 = addThing("Instance 1 2", model1)
  
  val instance21 = addThing("Instance 2 1", model2)
  
  val instance31 = addThing("Instance 3 1", model3,
    propOfModelType())
  val instance32 = addThing("Instance 3 2", model3)
  val instance33 = addThing("Instance 3 3", model3,
    propOfModelType(SimplePropertyBundle(
      textProp("3 3 Value"),
      numProp(24)
    )))
}

class ExtractAppTests extends QuerkiTests {
  
  lazy val Apps = interface[Apps]
  
  "An extracted App" should {
    "work normally" in {
      //
      // Step 1: do the extraction
      //
      val original = new SpaceToExtract
      val model1Id = original.model1.id
      val extractor = original.makeExtractor()
      
      try {
        extractor.extractApp(Seq(model1Id.toTID, original.model3.toTID), "My App")
      } catch {
        case ex:Exception => {
          QLog.error("Got an exception while trying to extract an app", ex)
          throw ex
        }
      }
      val appState = original.state.apps.head
      val appSpace = original.world.getSpace(appState.id).asInstanceOf[SpaceCoreSpaceBase]
      
//      println(s"The extracted App")
//      QLog.spewState(appState)
//      println(s"\nThe child State")
//      QLog.spewState(original.state)
      
      //
      // Step 2: test the extraction
      //
      {
        implicit val s = original
        implicit val childState = s.state
        val extractedByName = appState.anythingByName("Model 1").get
        val shadowModel = childState.localFrom(model1Id, childState.things).get
        assert(shadowModel.ifSet(Apps.ShadowFlag))
        assert(shadowModel.localProp(s.textProp.oid).isEmpty)
        val shadowModelParent = shadowModel.model
        val extractedById = appState.localFrom(shadowModelParent, appState.things).get
        
        pql("""[[Model 1._instances]]""") should equal(listOfLinkText(s.instance11, s.instance12))
        pql("""[[Instance 1 2 -> Text Property]]""") should equal("Default Text")
        
        val shadowComplexModel = childState.localFrom(s.model3.id, childState.things).get
        assert(shadowComplexModel.ifSet(Apps.ShadowFlag))
        assert(shadowComplexModel.localProp(s.propOfModelType.oid).isEmpty)
        val shadowMetaModel = childState.localFrom(s.modelForType.id, childState.things).get
        val shadowType = childState.localFrom(s.typeModel.t, childState.types).get
        shadowType match {
          case mt:ModelTypeBase => assert(mt.basedOn == shadowMetaModel.id)
          case _ => fail(s"$shadowType is not a ModelTypeBase!")
        }
        pql("""[[Instance 3 1 -> Complex Prop -> Int Property]]""") should equal("")
        pql("""[[Instance 3 2 -> Complex Prop -> Int Property]]""") should equal("17")
        pql("""[[Instance 3 3 -> Complex Prop -> Text Property]]""") should equal("3 3 Value")
      }
      
      //
      // Step 3: make a new Space based on the App
      //
      val newChild = new SpaceInWorldWith(original)
      newChild.addApp(appSpace)
      val newState = newChild.state
      
//      println(s"The derived child Space")
//      QLog.spewState(newState)
      
      //
      // Step 4: test the new Space
      //
      val localModel1 = newState.localFrom(AsName("Model 1"), newState.things).get
      val localChild11 = newChild.addThing("Local Instance 1 1", localModel1)
      val localChild12 = newChild.addThing("Local Instance 1 2", localModel1, original.textProp("Local Text"))
      
      val localModel3 = newState.localFrom(AsName("Model 3"), newState.things).get
      val localType = newState.localFrom(AsName("Type Model"), newState.types).get.asInstanceOf[ModelTypeDefiner#ModelType]
      val localRawProp = newState.localFrom(AsName("Complex Prop"), newState.spaceProps).get
      val localComplexProp = localRawProp.confirmType(localType).get
      val localChild31 = newChild.addThing("Local Instance 3 1", localModel3)
      val localChild32 = newChild.addThing("Local Instance 3 2", localModel3, localComplexProp(QList(localType(SimplePropertyBundle(
        original.textProp("Local 3 2 Value"),
        original.numProp(44)
      )))))
      
//      println(s"The enhanced derived child Space")
//      QLog.spewState(newChild.state)
      
      {
        implicit val s = newChild
        implicit val localState = newChild.state
        
        pql("""[[Local Instance 1 1 -> Text Property]]""") should equal("Default Text")
        pql("""[[Local Instance 1 2 -> Text Property]]""") should equal("Local Text")
        
        pql("""[[Local Instance 3 1 -> Complex Prop -> Int Property]]""") should equal("17")
        pql("""[[Local Instance 3 2 -> Complex Prop -> Int Property]]""") should equal("44")
      }
    }
  }
}
