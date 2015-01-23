package org.querki.facades

import org.scalajs.jquery._

package object fileupload {
  implicit def jq2FileUpload(jq:JQuery):FileUpload = jq.asInstanceOf[FileUpload]
  
  implicit def builder2FileUploadOptions(builder:FileUploadOptionBuilder) = builder.result
}
