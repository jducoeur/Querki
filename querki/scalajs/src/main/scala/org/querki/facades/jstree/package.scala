package org.querki.facades

import org.querki.jquery._

/**
 * @author jducoeur
 */
package object jstree {
  implicit def jq2JsTree(jq:JQuery):JsTree = jq.asInstanceOf[JsTree]
  implicit def jq2Commands(jq:JQuery):JsTreeCommands = new JsTreeCommands(jq)
  implicit def tree2Commands(jst:JsTree):JsTreeCommands = new JsTreeCommands(jst)
  implicit def tree2Events(jst:JsTree):JsTreeEvents = new JsTreeEvents(jst)
}
