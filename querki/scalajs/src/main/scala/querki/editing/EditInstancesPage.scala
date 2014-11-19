package querki.editing

import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.display.QText
import querki.pages._

class EditInstancesPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  val modelId = params.requiredParam("modelId") 

  def pageContent = {
    for {
      modelInfo <- DataAccess.getThing(modelId)
      editorWikitext <- Client[ThingFunctions].evaluateQL(modelId, "_edit").call()
      guts = 
        div(
          h3("Editing instances of ", thingLink(modelInfo)),
          new QText(editorWikitext),
          querkiButton(MSeq(href:=thingUrl(modelInfo), "Done"))
        )
    }
      yield PageContents(s"Editing Instances of ${modelInfo.displayName}", guts)
  }
}
