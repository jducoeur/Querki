package querki.api

import querki.globals._

trait CommonFunctionsImpl extends CommonFunctions { self:EcologyMember =>
  
  def getStandardInfo():StandardInfo = {
    StandardInfo(
      querki.conventions.MOIDs.PropDetailsOID.toThingId,
      querki.conventions.MOIDs.PropSummaryOID.toThingId,
      querki.core.MOIDs.UrPropOID.toThingId,
      querki.core.MOIDs.NameOID.toThingId,
      querki.core.MOIDs.CollectionPropOID.toThingId,
      querki.core.MOIDs.TypePropOID.toThingId,
      querki.basic.MOIDs.SimpleThingOID.toThingId,
      querki.core.MOIDs.IsModelOID.toThingId,
      querki.basic.MOIDs.DisplayNameOID.toThingId,
      querki.editing.MOIDs.InstanceEditPropsOID.toThingId
    )
  }
}
