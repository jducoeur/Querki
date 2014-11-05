package org.scalajs

import org.scalajs.jquery._

package object jqueryui {
  implicit def jq2UI(jq:JQuery):JQueryUIDialogFacade = jq.asInstanceOf[JQueryUIDialogFacade]
}
