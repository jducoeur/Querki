package querki.editing

import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.display.{PagePaginator, QText}
import querki.pages._

class EditInstancesPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  val modelId = params.requiredParam("modelId") 

  def pageContent = {
    for {
      modelInfo <- DataAccess.getThing(modelId)
      numInstances <- Client[ThingFunctions].getNumInstances(modelId).call()
      paginator = 
        new PagePaginator(
          numInstances,
          { newMap => PageManager.pageUrl("_editInstances", newMap) },
          params)
      editorWikitext <- Client[ThingFunctions].evaluateQL(modelId, s"_edit(${paginator.currentPage}, ${paginator.pageSize})").call()
      guts = 
        div(
          h3("Editing instances of ", thingLink(modelInfo), s" ($numInstances)"),
          paginator,
          new QText(editorWikitext),
          querkiButton(MSeq(href:=thingUrl(modelInfo), "Done"))
        )
    }
      yield PageContents(s"Editing Instances of ${modelInfo.displayName}", guts)
  }
}
