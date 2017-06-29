package querki

import models._
import querki.core.QLText
import querki.globals._

package object publication {
  trait Publication extends EcologyInterface {
    def CanPublishPermission:Property[OID, OID]
    def CanReadAfterPublication:Property[OID, OID]
    def PublishableModelProp:Property[Boolean, Boolean]
    def MinorUpdateProp:Property[Boolean, Boolean]
    def PublishedProp:Property[Boolean, Boolean]
    def HasUnpublishedChanges:Property[Boolean, Boolean]
    def PublishNotesProp:Property[QLText, String]
    def SpaceHasPublications:Property[Boolean, Boolean]
    
    def enhanceState(state:SpaceState, pub:CurrentPublicationState):SpaceState
  }
}
