package querki.editing

import scala.scalajs.js
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import autowire._
import org.querki.gadgets._

import querki.globals._

import querki.api.ThingFunctions
import EditFunctions.PropertyChange
import querki.data.ThingInfo
import querki.display.{HookedGadget, PagePaginator, QText}
import querki.pages._

/**
 * The Page for editing all instances of a Model
 * 
 * TODO: this is in a halfway state right now. Instead of letting the server compose the page, we
 * should go all the way, and change the API to have the Client request exactly the range of
 * instances it wants, request those, and have the server return a List of those instance editors.
 * We would then pull the rendering code from the CreateAnotherButton, and use that for all of them
 * in a nice consistent way.
 */
class EditInstancesPage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {
  
  lazy val Client = interface[querki.client.Client]
  
  val modelId = TID(params.requiredParam("modelId"))

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
          if (modelInfo.isInstantiatable) {
            div(
              new CreateAnotherButton(modelInfo),
              p(" ")
            )
          },
          div(querkiButton(MSeq(href:=thingUrl(modelInfo), "Done")))
        )
    }
      yield PageContents(s"Editing Instances of ${modelInfo.displayName}", guts)
  }
  
  /**
   * This button sits at the bottom of the page, and lets the user create a new instance.
   */
  class CreateAnotherButton(modelInfo:ThingInfo) extends HookedGadget[dom.HTMLInputElement](ecology) {
    def hook() = {
      $(elem).click({ event:JQueryEventObject => 
        val editorFut = for {
          thingInfo <- Client[EditFunctions].create(modelId, Seq.empty[PropertyChange]).call()
          thingEditor <- Client[ThingFunctions].evaluateQL(thingInfo.oid, "_edit").call()
        }
          yield thingEditor
          
        editorFut.foreach { editorWikitext =>
          val qt = new QText(editorWikitext)(ecology)
          val newEditor = qt.render
          $(newEditor).insertBefore($(elem))
          Gadgets.hookPendingGadgets()
          PageManager.instantScrollToBottom()
          Pages.updatePage(this)
          // TODO: is this the right way to do this in the new world? It begs for an abstraction: 
          $(newEditor).find(".propEditor").first().focus()
        }
      })
    }
  
    def doRender() =
      // TODO: this really should use plainQuerkiButton(); figure out how to get the types to align --
      input(
        tpe:="button",
        cls:="_createAnother btn btn-default btn-sm btn-primary querki-icon-button",
        value:=s"Create another ${modelInfo.displayName}"
      )
  }  
}
