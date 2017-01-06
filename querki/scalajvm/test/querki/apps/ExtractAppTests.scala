package querki.apps

import models.AsName
import querki.globals._
import querki.spaces.{ReplayCoreSpace, SpaceCoreSpaceBase}
import querki.spaces.messages.ThingError
import querki.test._
import querki.types.{ModelTypeBase, ModelTypeDefiner, SimplePropertyBundle}

class SpaceToExtract(implicit e:Ecology) extends AppableSpace {
  lazy val Basic = interface[querki.basic.Basic]
  
  val textProp = addProperty(Core.TextType, Core.Optional, "Text Property")
  val numProp = addProperty(Core.IntType, Core.ExactlyOne, "Int Property")
  
  val modelForType = addSimpleModel("Model for Type", textProp("Type Model Default"), numProp(12))
  val typeModel = addType("Type Model", modelForType)
  val propOfModelType = addProperty(typeModel, Core.QList, "Complex Prop")
  
  val page1 = addSimpleThing("Page 1", Basic.DisplayTextProp("Page 1 View"))
  
  val otherModel = addSimpleModel("Other Model")
  val other1 = addThing("Other 1", otherModel)
  
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
  
  def testChildSpace(original:SpaceToExtract, space:SpaceCoreSpaceBase, appState:SpaceState) = {
    implicit val s = space
    implicit val childState = s.state
    val extractedByName = appState.anythingByName("Model 1").get
    val shadowModel = childState.localFrom(original.model1.id, childState.things).get
    assert(shadowModel.ifSet(Apps.ShadowFlag))
    assert(shadowModel.localProp(original.textProp.oid).isEmpty)
    val shadowModelParent = shadowModel.model
    val extractedById = appState.localFrom(shadowModelParent, appState.things).get
    
    pql("""[[Model 1._instances]]""") should equal(listOfLinkText(original.instance11, original.instance12))
    pql("""[[Instance 1 2 -> Text Property]]""") should equal("Default Text")
    
    val shadowComplexModel = childState.localFrom(original.model3.id, childState.things).get
    assert(shadowComplexModel.ifSet(Apps.ShadowFlag))
    assert(shadowComplexModel.localProp(original.propOfModelType.oid).isEmpty)
    val shadowMetaModel = childState.localFrom(original.modelForType.id, childState.things).get
    val shadowType = childState.localFrom(original.typeModel.t, childState.types).get
    shadowType match {
      case mt:ModelTypeBase => assert(mt.basedOn == shadowMetaModel.id)
      case _ => fail(s"$shadowType is not a ModelTypeBase!")
    }
    pql("""[[Instance 3 1 -> Complex Prop -> Int Property]]""") should equal("")
    pql("""[[Instance 3 2 -> Complex Prop -> Int Property]]""") should equal("17")
    pql("""[[Instance 3 3 -> Complex Prop -> Text Property]]""") should equal("3 3 Value")
    pql("""[[Page 1 -> Default View]]""") should equal("Page 1 View")    
  }
  
  "An extracted App" should {
    "work normally" in {
      //
      // Step 1: do the extraction
      //
      val original = new SpaceToExtract
      val model1Id = original.model1.id
      val extractor = original.makeExtractor()
      
      try {
        extractor.extractApp(Seq(
          model1Id.toTID, 
          original.model3.toTID, 
          original.page1.toTID,
          original.otherModel.toTID,
          original.other1.toTID), 
        "My App",
        "My App Summary",
        "My App Details")
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
      
      val appVersion = original.state.appInfo.head._2
      assert(appVersion.v > 0)
      assert(appVersion == appState.version)
      assert(appVersion == appSpace.state.version)
      
      //
      // Step 2: test the extraction
      //
      testChildSpace(original, original, appState)
      
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
        
        // Test _isShadow and _shadowedThing
        pql("""[[Model 1 -> _isShadow]]""") should equal("true")
        // It's not defined on the shadowed Thing, so it comes out blank:
        pql("""[[Model 1 -> _shadowedThing -> _isShadow]]""") should equal("")
        // In other words, the Shadow and its _shadowedThing are not the same:
        pql("""[[Model 1 -> _shadowedThing -> _is(Model 1)]]""") should equal("false")
        // The local instance is *not* shadowed, so _shadowedThing produces itself:
        pql("""[[Local Instance 1 1 -> _shadowedThing -> _is(Local Instance 1 1)]]""") should equal("true")

        // Check that the Page works, and is shadowed:
        pql("""[[Page 1 -> Default View]]""") should equal("Page 1 View")
        pql("""[[Page 1 -> _shadowedThing -> _is(Page 1)]]""") should equal("false")
      }
      
      // Make sure we can edit Page 1:
      val localPage1 = newState.localFrom(AsName("Page 1"), newState.things).get
      newChild.changeThing(localPage1, Basic.DisplayTextProp("Child Page 1 View"))
      
      {
        implicit val s = newChild
        implicit val localState = newChild.state
        
        pql("""[[Page 1 -> Default View]]""") should equal("Child Page 1 View")
      }
      
      // The non-Page Instance should *not* be shadowed, and should not be editable:
      assert(newState.localFrom(AsName("Other 1"), newState.things).isEmpty)
      val other1 = newChild.state.anythingByName("Other 1").get
      newChild.changeThing(other1, Basic.DisplayTextProp("This should fail!")) match {
        case Some(ThingError(ex, _)) => assert(ex.msgName == "Thing.find.noSuch")
        case other => fail(s"Changing Other 1 should have failed, but returned $other!")
      }

      // TODO: Apps are no longer auto-enrolled in the Gallery, so this test doesn't work.
      // When we add voluntary enrollment, try that out here:
//      // Check that it got entered into the App Gallery:
//      val entryId = appState.first(Apps.GalleryEntryId)(appState)
//      
//      {
//        implicit val g = newChild.world.getSpace(MOIDs.GallerySpaceOID)
//        implicit val gState = g.state
//        
//        // Check that the App and Gallery agree about the entry ID:
//        gState.anything(entryId) match {
//          case Some(entry) => {
//            entry.first(Basic.DisplayNameProp).text should equal ("My App")
//          }
//          case None => fail(s"Gallery doesn't contain an entry numbered $entryId!")
//        }
//        
//        pql("""[[_App Gallery Entry._instances -> Name]]""") should equal("My App")
//        pql("""[[_App Gallery Entry._instances -> _Space Summary]]""") should equal("My App Summary")
//        pql("""[[_App Gallery Entry._instances -> _Space Details]]""") should equal("My App Details")
//        pql("""[[_App Gallery Entry._instances -> _App Gallery Owner -> _oid]]""") should equal(appState.owner.toThingId.toString)
//        pql("""[[_App Gallery Entry._instances -> _App Gallery App Id -> _oid]]""") should equal(appState.id.toThingId.toString)
//      }
      
      //
      // Step 5: check that reloading the extracted child Space still works:
      //
      val replayedChild = new ReplayCoreSpace(original)
      testChildSpace(original, replayedChild, appState)
    }
  }
}
