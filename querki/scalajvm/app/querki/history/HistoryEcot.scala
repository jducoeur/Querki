package querki.history

import querki.ecology._
import querki.globals._

class HistoryEcot(e:Ecology) extends QuerkiEcot(e) {
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[HistoryFunctions, HistoryFunctionsImpl](SpaceOps.spaceRegion)
  }
}
