import org.scalajs.jquery._
import org.scalajs.ext._

package object raty {
  /**
   * For now, this is simply a documentation marker. But we might flesh this out to become a
   * stronger Type down the road.
   */
  type Selector = String
  
  implicit def jq2Boostrap(jq:JQuery):RatyFacade = jq.asInstanceOf[RatyFacade]
  implicit def builder2RatyOptions(builder:RatyOptionBuilder) = builder.result
}