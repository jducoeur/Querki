package querki.html

import querki.test._

import play.api.data._
import play.api.data.Forms._

import models.DisplayPropVal
import querki.basic.PlainText
import querki.types.ModelTypeDefiner
import querki.values.QLRequestContext

class RendererTests extends QuerkiTests {
  "handleTagSet" should {
    "cope with nested tags" in {
      // This test is basically testing what happens when Application gets a Tag value in a nested Model,
      // and passes it to HtmlRenderer:
      lazy val Tags = interface[querki.tags.Tags]
      lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
      
      class NestedSpace extends CommonSpace with ModelTypeDefiner {
        val tagModel = new SimpleTestThing("Tag Model")
        val tagVal1 = new TestThing("Tag Value 1", tagModel)
        val tagVal2 = new TestThing("Tag Value 2", tagModel)
        val tagVal3 = new TestThing("Tag Value 3", tagModel)
        val tagProp = new TestProperty(Tags.NewTagSetType, QSet, "Tag Property")
        
        val bottomModel = new SimpleTestThing("Bottom Model", tagProp("Tag Value 2"))
        val bottomType = new ModelType(toid, bottomModel.id, Core.toProps())
        registerType(bottomType)
        val bottomProp = new TestProperty(bottomType, Optional, "Bottom Property")
        
        val midModel = new SimpleTestThing("Middle Model", bottomProp())
        val midType = new ModelType(toid, midModel.id, Core.toProps())
        registerType(midType)
        val midProp = new TestProperty(midType, ExactlyOne, "Middle Property")

        val topModel = new SimpleTestThing("Top Model", midProp())
        val myInstance = new TestThing("My Instance", topModel)
      }
      implicit val s = new NestedSpace
      implicit val state = s.state
      
      // Hand-construct the field name that we expect to get from the browser:
      val fieldName = s"v-${s.midProp.id.toString}-${s.bottomProp.id.toString}-${s.tagProp.id.toString}-${s.myInstance.id.toString}"
      val fieldIds = DisplayPropVal.propPathFromName(fieldName, Some(s.myInstance))
      assert(fieldIds.isDefined)
      val form = new Form(
          mapping("dummy" -> text)((dummy) => dummy)((dummy:String) => Some(dummy)), 
          Map((fieldName + "_values[0]" -> "Tag Value 1"), (fieldName + "_values[1]" -> "Tag Value 3")), 
          Seq(), None)
      val context = QLRequestContext(getRc)
      
      val formFieldInfo = HtmlRenderer.propValFromUser(fieldIds.get, Some(s.myInstance), form, context)
      
      assert(formFieldInfo.prop == s.tagProp)
      assert(!formFieldInfo.isEmpty)
      assert(formFieldInfo.isValid)
      
      val v = formFieldInfo.value.get
      assert(v.contains(Tags.NewTagSetType, PlainText("Tag Value 1")))
      assert(v.contains(Tags.NewTagSetType, PlainText("Tag Value 3")))
      assert(!v.contains(Tags.NewTagSetType, PlainText("Tag Value 2")))
    }
  }
}