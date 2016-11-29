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
import querki.display.{ButtonGadget, Gadget, QText}
import querki.display.rx._
import RxEmptyable._
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
  val summaryInput = GadgetRef[RxText]
  val detailsInput = GadgetRef[RxTextArea]
  
  val notReady = Rx { 
    appNameInput.rxEmpty() ||
    summaryInput.rxEmpty() ||
    detailsInput.rxEmpty()
  }
  
  def pageContent = {
    for {
      modelsUnsorted <- Client[AppsFunctions].getExtractableModels().call()
      models = modelsUnsorted.sortBy(_.displayName)
      pages <- Client[ThingFunctions].getChildren(std.basic.simpleThing, false, true).call()
      sortedPages = pages.sortBy(_.displayName)
      guts = 
        div(
          h1("Extract an App"),
          QText("""This page lets you create an **App** based on this Space. You do this by "extracting" the structure
                  |of the Space into the App, while leaving the data here. Afterwards, your Space will look essentially the
                  |same, but you will now have an App that can be shared with others.
                  |
                  |For the time being, all Apps are public, and will appear in Querki's public App Gallery. By using this
                  |feature, you are agreeing to let Querki share this App with everyone.""".stripMargin),
          p("What should the new App be named?"),
          appNameInput <= new RxText(),
          p("Briefly describe the new App"),
          summaryInput <= new RxText(),
          p("Give a bit more description of the new App"),
          detailsInput <= new RxTextArea(),
          p("In the list below, select the Models and Instances to lift into the new App."),
          p("""Initially, all Models and all Pages (Things based on Simple Thing) are selected. That is often just what you want,
              |but you can uncheck anything you don't want to have in the new App. You can also open a Model and add some or all of
              |its Instances. In general, you want to includes all Things that are part of the structure of this App, but not
              |the ones that are part of the data of this Space.""".stripMargin),
          p("Any local Properties that are used by these Models and Pages will automatically be lifted into the App"),
          extractTree <= new ExtractTree(models, sortedPages),
          p("When you have selected the elements you would like to bring into the App, press this button."),
          new ButtonGadget(ButtonGadget.Warning, "Extract App from this Space", disabled := notReady)
          ({ () =>
            val jq = $(extractTree.get.elem)
            val selectedIds = jq.getSelectedIds.map(TID(_))
            Client[AppsFunctions].extractApp(selectedIds, appNameInput.get.text(), summaryInput.get.text(), detailsInput.get.text()).call() foreach { _ =>
              Pages.infoFactory.showPage()
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
      (if (model.canExtract) Seq(NodeState.Selected) else Seq(NodeState.Disabled)) ++
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
