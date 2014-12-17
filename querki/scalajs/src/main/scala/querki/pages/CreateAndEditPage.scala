package querki.pages

import scalatags.JsDom.all.{input => inp, _}
import autowire._

import querki.globals._

import querki.api.{EditFunctions, ThingFunctions}
import querki.api.EditFunctions.{ChangePropertyValue, PropertyChange}
import querki.display.QText

class CreateAndEditPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]
  
  lazy val modelId = TID(params("model"))

  def pageContent = for {
    modelInfo <- Client[ThingFunctions].getThingInfo(modelId).call()
    // TODO: pick up initial values from the params
    thingInfo <- Client[EditFunctions].create(modelId, initialValues).call()
    dummy = {
      DataSetting.setThing(Some(thingInfo))
      DataSetting.setModel(Some(modelInfo))
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
}
