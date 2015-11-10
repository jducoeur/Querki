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
    // There is clearly a higher-level combinator fighting to break out here, probably in JSOptionBuilder:
    val withText = JsTreeNode.text(span.html())
    val withOpened = 
      if (span.data("opened").get.asInstanceOf[Boolean])
        withText.state(NodeState.Opened)
      else
        withText
    val withIcon = 
      span.data("icon").map { icon => withOpened.icon(icon.asInstanceOf[String]) }.getOrElse(
      span.data("showicon").map { showIcon => withOpened.icon(showIcon.asInstanceOf[Boolean]) }.getOrElse(withOpened))
    val tid = span.tidString("thingid")
    val qlNode = span.find("._treeQL") 
    val withData = 
      qlNode.mapElems(qle => $(qle).text).headOption match {
        case Some(ql) => withIcon.children(true).data(NodeData(Some(ql), tid))
        case None => withIcon.data(NodeData(None, tid))
      }
    qlNode.remove()
    withData
  }
  
  def hook() = {
    val node = dissectSpan(elem)
//    spew("The node gets built as", node)
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
