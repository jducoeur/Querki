package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._
import querki.data.ThingInfo
import querki.display.RawSpan
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val modelId = params("modelId")
  
  lazy val Client = interface[querki.client.Client]
  
  def makeEditor(info:PropEditInfo):Modifier = {
    div(
      s"${info.displayName} (${info.propId}): ",
      new RawSpan(info.editor)
    )
  }

  def pageContent = {
    for {
      standardInfo <- DataAccess.standardInfo
      model <- DataAccess.getThing(modelId)
      editors <- Client[EditFunctions].getThingEditors(modelId).call()
      guts = 
        div(
          editors.propInfos.map(makeEditor(_)),
          h4(s"Instance Props: ${editors.instancePropIds.mkString(", ")}")
        )
    }
      yield PageContents("TODO: title", guts)
  }
  
}
