package querki.apps

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.dom

import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.jquery._
import org.querki.facades.jstree._

import querki.api.ThingFunctions
import querki.data.{ThingInfo}
import querki.display.{ButtonGadget, Gadget}
import querki.display.rx._
import querki.globals._
import querki.pages.{IndexPage, Page, PageContents, ParamMap}

/**
 * @author jducoeur
 */
class ExtractAppPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  
  val extractTree = GadgetRef[ExtractTree]
  
  def pageContent = {
    for {
      allTypeInfo <- Client[ThingFunctions].getAllTypes().call()
      models = allTypeInfo.models.sortBy(_.displayName)
      pages <- Client[ThingFunctions].getChildren(std.basic.simpleThing, false, true).call()
      sortedPages = pages.sortBy(_.displayName)
      guts = 
        div(
          h1("Extract an App"),
          p("In the list below, select the Models and Instances to lift into the new App."),
          p("""Initially, all Models and all Pages (Things based on Simple Thing) are selected. That is often just what you want,
              |but you can uncheck anything you don't want to have in the new App. You can also open a Model and add some or all of
              |its Instances. In general, you want to includes all Things that are part of the structure of this App, but not
              |the ones that are part of the data of this Space.""".stripMargin),
          p("Any local Properties that are used by these Models and Pages will automatically be lifted into the App"),
          extractTree <= new ExtractTree(models, pages),
          p("When you have selected the elements you would like to bring into the App, press this button."),
          new ButtonGadget(ButtonGadget.Warning, "Extract App from this Space")({ () =>
            val jq = $(extractTree.get.elem)
            val selectedIds = jq.getSelectedIds.map(TID(_))
            Client[AppsFunctions].extractApp(selectedIds).call() foreach { progressMonitor =>
              // TODO -- this should deal with the progress bar
            }
          }),
          new ButtonGadget(ButtonGadget.Normal, "Cancel")({() => Pages.showSpacePage(DataAccess.space.get)})
        )
    }
      yield PageContents("Extract App", guts)
  }
}

class ExtractTree(models:Seq[ThingInfo], pages:Seq[ThingInfo])(implicit val ecology:Ecology) extends Gadget[dom.html.Div] with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  // TODO: put child Models underneath their parents. Models should always be opened, with an
  // "Instances" node beneath them that fetches their Instances.
  val modelNodes = models.map { model =>
    JsTreeNode
      .text(s"<b>${model.displayName}</b>")
      .icon(false)
      .children(true)
      .id(model.oid.underlying)
      // By default, all Models are selected:
      .state(NodeState.Selected)
      :JsTreeNode
  }
  
  val pageNodes = pages.map { page =>
    JsTreeNode
      .text(s"${page.displayName}")
      .icon(false)
      .children(false)
      .id(page.oid.underlying)
      // By default, all Pages are selected:
      .state(NodeState.Selected)
      :JsTreeNode
  }
  
  val initNodes = (modelNodes ++ pageNodes) 

  def doRender() = {
    div(
      cls:="_extractAppTree"
    )
  }
  
  override def onCreate(e:dom.html.Div) = {
    $(e).jsTree(JsTreeOptions
      .plugins(JsTreePlugins.Checkbox)
      .core(JsTreeCore
        .worker(false)
        .themes(JsTreeTheme
          .dots(false)
          .responsive(true))
        .data({ (asNode:JsTreeNode, cb:js.Function1[js.Array[JsTreeNode], Any]) =>
          val id = asNode.id.get
          if (id == "#") {
            cb(initNodes.toJSArray)
          } else {
            val tid = TID(id)
            Client[ThingFunctions].getChildren(tid, false, true).call() foreach { result =>
              val instanceNodes = result map { instanceInfo =>
                JsTreeNode
                  .text(s"${instanceInfo.displayName}")
                  .icon(false)
                  .children(false)
                  .id(instanceInfo.oid.underlying)
                  :JsTreeNode
              }
              cb(instanceNodes.toJSArray)
            }
          }
        })
      )
      .checkbox(JsTreeCheckbox
        .threeState(false)
        .keepSelectedStyle(false)
      )
    )
  }
}
