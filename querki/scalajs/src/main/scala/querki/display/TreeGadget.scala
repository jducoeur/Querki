package querki.display

import scala.scalajs.js
import js.JSConverters._
import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._

import org.querki.jquery._
import org.querki.facades.jstree._

import querki.api.ThingFunctions
import querki.globals._

/**
 * @author jducoeur
 */
class TreeGadget(implicit e:Ecology) extends HookedGadget[dom.html.Div](e) {
  def doRender() = ???
  
  def hook() = {
    $(elem).jsTree(JsTreeOptions.
      core(JsTreeCore.
        themes(JsTreeTheme.
          dots(false)
        )
      ))
  }
}

case class NodeData(ql:Option[String], thingId:TID)

class QLTree(implicit e:Ecology) extends HookedGadget[dom.html.Div](e) {

  lazy val Client = interface[querki.client.Client]
  
  def doRender() = ???
  
  def dissectSpan(e:dom.Element):JsTreeNode = {
    val span = $(e)

    // This mechanism might yet break out to be something more general, but its
    // relationship to the span makes that tricker. In the long run, we might be looking
    // instead at some data-binding mechanism between the span and the JsTreeNodeBuilder?
    implicit class JsTreeNodeExt(node:JsTreeNodeBuilder) {
      def ifdata(name:String)(func:(JsTreeNodeBuilder, js.Any) => JsTreeNodeBuilder):JsTreeNodeBuilder = {
        span.data(name).map { data => func(node, data) }.getOrElse(node)
      }
    }
    
    val tid = span.tidString("thingid")
    
    val node = JsTreeNode.text(span.html())
      .ifdata("id") { (node, id) => node.id(id.asInstanceOf[String]) }
      .ifdata("opened") { (node, opened) => 
        if (opened.asInstanceOf[Boolean]) 
          node.state(NodeState.Opened)
        else
          node
      }
      .ifdata("icon") { (node, icon) => node.icon(icon.asInstanceOf[String]) }
      .ifdata("showicon") { (node, showIcon) => node.icon(showIcon.asInstanceOf[Boolean]) }
      
    val qlNode = span.find("._treeQL") 
    val withData = 
      qlNode.mapElems(qle => $(qle).text).headOption match {
        case Some(ql) => node.children(true).data(NodeData(Some(ql), tid))
        case None => node.data(NodeData(None, tid))
      }
    qlNode.remove()
    withData
  }
  
  def hook() = {
    val node = dissectSpan(elem)
    val tree = div(cls:="_qlTreeRoot").render
    $(tree).insertBefore(elem)
    $(tree).jsTree(JsTreeOptions.
      core(JsTreeCore.
        // We are turning off workers, because they are causing weird crashes, I think:
        worker(false).
        data({ (nodeIn:js.Object, cb:js.Function1[js.Array[JsTreeNode], Any]) =>
          val asNode = nodeIn.asInstanceOf[JsTreeNode]
          if (asNode.id == "#") {
            cb(js.Array(node))
          } else {
            val nodeData = asNode.data.asInstanceOf[NodeData]
            nodeData.ql match {
              case Some(ql) => {
                Client[ThingFunctions].evaluateQL(nodeData.thingId, ql).call().foreach { result =>
                  val rendered = span(raw(result.raw)).render
                  val childNodes = $(rendered).find("._qlTree").mapElems(dissectSpan)
                  cb(childNodes.toJSArray)
                }
              }
              case _ => cb(js.Array())
            }

          }
        }).
        themes(JsTreeTheme.
          dots(false))
      )
    )
    $(elem).remove()
    setElem(tree)
  }
}
