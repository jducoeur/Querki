package querki.apps

import querki.globals._
import querki.test._

class SpaceToExtract(implicit e:Ecology) extends AppableSpace {
  val textProp = addProperty(Core.TextType, Core.Optional, "Text Property")
  
  val model1 = addSimpleModel("Model 1", textProp("Default Text"))
  val model2 = addSimpleModel("Model 2")
  
  val instance11 = addThing("Instance 1 1", model1, textProp("Hello from Instance 1 1!"))
  val instance12 = addThing("Instance 1 2", model1)
  
  val instance21 = addThing("Instance 2 1", model2)
}

class ExtractAppTests extends QuerkiTests {
  
  lazy val Apps = interface[Apps]
  
  "An extracted App" should {
    "work in a simple case" in {
      implicit val s = new SpaceToExtract
      val model1Id = s.model1.id
      val extractor = s.makeExtractor()
      
      extractor.extractApp(Seq(model1Id.toTID), "My App")      
      val appState = s.state.apps.head
      implicit val childState = s.state
      
//      println(s"The extracted App")
//      QLog.spewState(appState)
//      println(s"\nThe child State")
//      QLog.spewState(childState)
      
      val extractedByName = appState.anythingByName("Model 1").get
      val shadowModel = childState.localFrom(model1Id, childState.things).get
      assert(shadowModel.ifSet(Apps.ShadowFlag))
      assert(shadowModel.localProp(s.textProp.oid).isEmpty)
      val shadowModelParent = shadowModel.model
      val extractedById = appState.localFrom(shadowModelParent, appState.things).get
      
      pql("""[[Model 1._instances]]""") should equal(listOfLinkText(s.instance11, s.instance12))
      pql("""[[Instance 1 2 -> Text Property]]""") should equal("Default Text")
    }
  }
}
