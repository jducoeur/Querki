package org.querki.facades

import org.querki.jquery._
import org.querki.jsext._

package object raty {
  /**
   * For now, this is simply a documentation marker. But we might flesh this out to become a
   * stronger Type down the road.
   */
  type Selector = String
  
  implicit def jq2Boostrap(jq:JQuery):RatyFacade = jq.asInstanceOf[RatyFacade]
  implicit def builder2RatyOptions(builder:RatyOptionBuilder) = builder._result
}