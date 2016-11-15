package querki.apps

import querki.globals._
import querki.spaces.SpaceCoreSpaceBase
import querki.test._
import querki.types.{ModelTypeBase, SimplePropertyBundle}

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
      implicit val s = new SpaceToExtract
      val model1Id = s.model1.id
      val extractor = s.makeExtractor()
      
      try {
        extractor.extractApp(Seq(model1Id.toTID, s.model3.toTID), "My App")
      } catch {
        case ex:Exception => {
          QLog.error("Got an exception while trying to extract an app", ex)
          throw ex
        }
      }
      val appState = s.state.apps.head
      val appSpace = s.world.getSpace(appState.id).asInstanceOf[SpaceCoreSpaceBase]
      implicit val childState = s.state
      
//      println(s"The extracted App")
//      QLog.spewState(appState)
//      println(s"\nThe child State")
//      QLog.spewState(childState)
      
      //
      // Step 2: test the extraction
      //
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
      
      //
      // Step 3: make a new Space based on the App
      //
      val newChild = new SpaceInWorldWith(s)
      newChild.addApp(appSpace)
    }
  }
}
