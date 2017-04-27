package querki

import models._
import querki.globals._

package object publication {
  trait Publication extends EcologyInterface {
    def CanPublishPermission:Property[OID, OID]
    def CanReadAfterPublication:Property[OID, OID]
    def PublishableModelProp:Property[Boolean, Boolean]
    def MinorUpdateProp:Property[Boolean, Boolean]
  }
}
