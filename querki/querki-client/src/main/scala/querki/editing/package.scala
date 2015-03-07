package querki

import querki.globals._

import querki.pages.{PageFactory, ThingPageFactory}

package object editing {
  trait Editing extends EcologyInterface {
    /**
     * The page that lets you edit all of the instances of a given Model.
     */
    def editInstancesFactory:ThingPageFactory
    
    /**
     * The page that lets you edit a given Model.
     */
    def modelDesignerFactory:ThingPageFactory
    
    /**
     * The Advanced Editor for non-Models.
     */
    def advancedEditorFactory:ThingPageFactory
    
    /**
     * Constructs a server-compatible "path" to identify a Property.
     */
    def propPath(propId:TID, thingIdOpt:Option[TID]):String
    def propPath(propId:TID):String
    /**
     * Like the name says -- this is a workaround while we are evolving the API.
     * 
     * TODO: this is only necessary because the server is still generating paths including
     * raw OIDs that don't start with a dot. Fix that, and this should be able to go away.
     */
    def propPathOldStyleHack(propId:TID, thingIdOpt:Option[TID]):String
  }
}
