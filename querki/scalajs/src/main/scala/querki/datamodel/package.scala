package querki

import rx._

import querki.globals._

import querki.data.ThingInfo

package object datamodel {
  trait DataModel extends EcologyInterface {
    /**
     * Delete the specified Thing.
     */
    def deleteAfterConfirm(thing:ThingInfo):Unit
    
    /**
     * Pop a standard dialog that allows the user to choose a model.
     */
    def chooseAModel(title:String, msg:String, buttonText:String)(implicit ctx:Ctx.Owner):Future[Option[TID]]
    
    /**
     * Displays a dialog to let the user choose a new Model for the Thing, makes the
     * change, and calls cb().
     */
    def changeModel(thing:ThingInfo, cb:ThingInfo => Unit)(implicit ctx:Ctx.Owner):Unit
  }
}
