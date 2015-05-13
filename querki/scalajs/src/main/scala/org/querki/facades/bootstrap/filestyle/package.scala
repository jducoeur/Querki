package org.querki.facades.bootstrap

import org.querki.jquery.JQuery

package object filestyle {
  implicit def jq2Filestyle(jq:JQuery):BootstrapFilestyle = jq.asInstanceOf[BootstrapFilestyle]
}
