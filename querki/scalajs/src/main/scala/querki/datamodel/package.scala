package querki

import querki.globals._

import querki.data.ThingInfo

package object datamodel {
  trait DataModel extends EcologyInterface {
    /**
     * Delete the specified Thing.
     */
    def deleteAfterConfirm(thing:ThingInfo):Unit
    
    /**
     * Creates a new Model, and opens the Model Designer on it.
     */
    def designAModel():Unit
  }
}
