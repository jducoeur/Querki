package org.querki.facades

import org.querki.jquery._

package object fileupload {
  implicit def jq2FileUpload(jq: JQuery): FileUpload = jq.asInstanceOf[FileUpload]
}
