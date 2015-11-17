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
import querki.display.Gadget
import querki.globals._
import querki.pages.{IndexPage, Page, PageContents, ParamMap}

/**
 * @author jducoeur
 */
class ExtractAppPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  
  def pageContent = {
    for {
      allTypeInfo <- Client[ThingFunctions].getAllTypes().call()
      models = allTypeInfo.models.sortBy(_.displayName)
      pages <- Client[ThingFunctions].getChildren(std.basic.simpleThing, false, true).call()
      sortedPages = pages.sortBy(_.displayName)
      guts = 
        div(
          h1("Extract an App"),
          new ExtractTree(models, pages)
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
      .plugins(Seq(JsTreePlugins.Checkbox))
      .core(JsTreeCore
        .worker(false)
        .themes(JsTreeTheme
          .dots(false)
          .responsive(true))
        .data({ (nodeIn:js.Object, cb:js.Function1[js.Array[JsTreeNode], Any]) =>
          val asNode = nodeIn.asInstanceOf[JsTreeNode]
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
