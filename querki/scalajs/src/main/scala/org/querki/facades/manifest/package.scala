package org.querki.facades

import org.querki.jquery.JQuery

package object manifest {
  implicit def jq2MarcoPolo(jq:JQuery):MarcoPoloFacade = jq.asInstanceOf[MarcoPoloFacade]
  implicit def builder2MarcoPoloOptions(builder:MarcoPoloOptionBuilder) = builder.result

  implicit def jq2Manifest(jq:JQuery):ManifestFacade = jq.asInstanceOf[ManifestFacade]
  implicit def builder2ManifestOptions(builder:ManifestOptionBuilder) = builder.result
}
