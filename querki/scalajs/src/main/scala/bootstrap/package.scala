import org.scalajs.jquery._

import org.scalajs.ext._

package object bootstrap {
  implicit def jq2Boostrap(jq:JQuery):BootstrapFacade = jq.asInstanceOf[BootstrapFacade]
  
  implicit def builder2PopoverOptions(builder:PopoverOptionBuilder) = builder.result
}
