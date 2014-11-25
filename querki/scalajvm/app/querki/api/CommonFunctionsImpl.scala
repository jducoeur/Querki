package querki.api

import querki.globals._

trait CommonFunctionsImpl extends CommonFunctions { self:EcologyMember =>
  def getStandardInfo():StandardInfo = {
    StandardInfo()
  }
}