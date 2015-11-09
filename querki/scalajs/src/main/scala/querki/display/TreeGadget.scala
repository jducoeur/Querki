package querki.display

import scala.scalajs.js
import org.scalajs.dom

import scalatags.JsDom.all._

import org.querki.jquery._
import org.querki.facades.jstree._

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

class QLTree(implicit e:Ecology) extends HookedGadget[dom.html.Div](e) {
  def doRender() = ???
  
  def dissectSpan(e:dom.Element):JsTreeNode = {
    val span = $(e)
    val withText = JsTreeNode.text(span.html())
    val withOpened = 
      if (span.data("opened").get.asInstanceOf[Boolean])
        withText.state(NodeState.Opened)
      else
        withText
    val withIcon = span.data("icon").map { icon => withOpened.icon(icon.asInstanceOf[String]) }.getOrElse(withOpened)
    val withQL = span.data("ql").map { ql => withIcon.children(true).data(ql) }.getOrElse(withIcon)
    withQL
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
            println(s"It's a child node")
            // Invoke the QL
          }
        }).
        themes(JsTreeTheme
          )//dots(false))
      )
    )
    $(elem).remove()
    setElem(tree)
  }
}
