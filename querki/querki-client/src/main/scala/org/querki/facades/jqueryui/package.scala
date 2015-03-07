package org.querki.facades

import org.scalajs.jquery._
import org.querki.jsext._

package object jqueryui {
  /**
   * For now, this is simply a documentation marker. But we might flesh this out to become a
   * stronger Type down the road.
   */
  type Selector = String
  
  implicit def jq2UI(jq:JQuery):JQueryUIDialogFacade = jq.asInstanceOf[JQueryUIDialogFacade]
  implicit def jq2Sortable(jq:JQuery):JQueryUISortableFacade = jq.asInstanceOf[JQueryUISortableFacade]
  
  implicit def builder2SortableOptions(builder:SortableOptionBuilder) = builder.result
  implicit def builder2DialogOptions(builder:DialogOptionBuilder) = builder.result
}
