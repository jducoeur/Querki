package querki.pages

import scalatags.JsDom.all.{input => inp, _}
import autowire._

import querki.globals._

import querki.api.{EditFunctions, ThingFunctions}
import querki.api.EditFunctions.{ChangePropertyValue, PropertyChange}
import querki.data.ThingInfo
import querki.display.QText
import querki.display.input.InputGadget

class CreateAndEditPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val Editing = interface[querki.editing.Editing]
  
  lazy val modelId = TID(params("model"))
  
  var thingInfoOpt:Option[ThingInfo] = None

  def pageContent = for {
    modelInfo <- Client[ThingFunctions].getThingInfo(modelId).call()
    // TODO: pick up initial values from the params
    thingInfo <- Client[EditFunctions].create(modelId, initialValues).call()
    dummy = {
      DataSetting.setThing(Some(thingInfo))
      DataSetting.setModel(Some(modelInfo))
      thingInfoOpt = Some(thingInfo)
    }
    editor <- Client[ThingFunctions].evaluateQL(thingInfo.oid, "_edit").call()
    guts = div(new QText(editor))
  }
    yield PageContents(s"Create a ${modelInfo.displayName}", guts)
    
  def initialValues:Seq[PropertyChange] = {
    val otherParams = params - "model"
    val changes = otherParams.map { pair =>
      val (path, v) = pair
      ChangePropertyValue(path, List(v))
    }
    changes.toSeq
  }
    
  /**
   * First pass at allowing other systems to inject values into a newly-created page.
   * 
   * TODO: this is very crude in many ways, but I'm not sure yet what the right approach is.
   * It is mainly intended as a way to create a page and set a Large Text value, which we don't
   * really want to do through the URL. Conceptually, it's like an internal POST form submission.
   * It is currently used from the Explore page.
   */
  def setValue(prop:TID, v:String) = {
    // Controls are identified by their path. Assuming the desired Property is here, its path
    // should exist as an ID:
    val path = Editing.propPathOldStyleHack(prop, thingInfoOpt.map(_.oid))
    val control = $(elem).find(s"#$path")
    findGadgets(control).foreach {
      case gadget:InputGadget[_] => {
        gadget.setValue(v)
      }
    }
  }
}
