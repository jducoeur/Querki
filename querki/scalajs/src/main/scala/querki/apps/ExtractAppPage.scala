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

import AppsFunctions._

/**
 * @author jducoeur
 */
class ExtractAppPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Apps = interface[Apps]
  lazy val Client = interface[querki.client.Client]
  lazy val ProgressDialog = interface[querki.display.ProgressDialog]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  val extractTree = GadgetRef[ExtractTree]
  val appNameInput = GadgetRef[RxText]
  val appNameEmpty = Rx { appNameInput.flatMap(_.textOpt()).map { _.isEmpty }.getOrElse(true) }
  
  def pageContent = {
    for {
      modelsUnsorted <- Client[AppsFunctions].getExtractableModels().call()
      models = modelsUnsorted.sortBy(_.displayName)
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
          p("What should the new App be named?"),
          appNameInput <= new RxText(),
          p("When you have selected the elements you would like to bring into the App, press this button."),
          new ButtonGadget(ButtonGadget.Warning, "Extract App from this Space", disabled := appNameEmpty)
          ({ () =>
            val jq = $(extractTree.get.elem)
            val selectedIds = jq.getSelectedIds.map(TID(_))
            Client[AppsFunctions].extractApp(selectedIds, appNameInput.get.text()).call() foreach { handle =>
              ProgressDialog.showDialog(
                s"extracting App", 
                handle, 
                Apps.appMgmtFactory.showPage(), 
                StatusLine.showBriefly(s"Error while extracting an App!"))              
            }
          }),
          new ButtonGadget(ButtonGadget.Normal, "Cancel")({() => Pages.showSpacePage(DataAccess.space.get)})
        )
    }
      yield PageContents("Extract App", guts)
  }
}

class ExtractTree(models:Seq[ExtractableModelInfo], pages:Seq[ThingInfo])(implicit val ecology:Ecology) extends Gadget[dom.html.Div] with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val space = DataAccess.space.get
  val spaceNode =
    JsTreeNode
      .text(s"<b><i>Space Root:</i> ${space.displayName}</b>")
      .icon(false)
      .children(false)
      .id(space.oid.underlying)
      // By default, the Space Root is selected:
      .state(NodeState.Selected)
      :JsTreeNode
  
  // TODO: put child Models underneath their parents.
  val modelNodes = models.map { model =>
    val state:Seq[NodeState] =
      (if (model.canExtract) Seq(NodeState.Selected) else Seq.empty) ++
      (if (model.extractInstancesByDefault) Seq(NodeState.Opened) else Seq.empty)
        
    JsTreeNode
      .text(s"<b>${model.displayName}</b>")
      .icon(false)
      .children(true)
      .id(model.oid.underlying)
      .state(state:_*)
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
  
  val initNodes = (spaceNode +: (modelNodes ++ pageNodes)) 

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
                val model = models.find(_.oid == tid).get
                val instanceState = 
                  if (model.extractInstancesByDefault && instanceInfo.importedFrom.isEmpty)
                    Seq(NodeState.Selected)
                  else
                    Seq.empty
                JsTreeNode
                  .text(s"${instanceInfo.displayName}")
                  .icon(false)
                  .children(false)
                  .id(instanceInfo.oid.underlying)
                  .state(instanceState:_*)
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
