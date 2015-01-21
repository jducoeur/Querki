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
    
    /**
     * Allows the user to select a Model, creates a new Instance, and opens the editor for it.
     */
    def createAThing():Unit
    
    /**
     * Displays a dialog to let the user choose a new Model for the Thing, makes the
     * change, and calls cb().
     */
    def changeModel(thing:ThingInfo, cb:ThingInfo => Unit):Unit
  }
}
