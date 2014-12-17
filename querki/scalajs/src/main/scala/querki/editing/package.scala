package querki

import querki.globals._

import querki.pages.PageFactory

package object editing {
  trait Editing extends EcologyInterface {
    /**
     * The page that lets you edit all of the instances of a given Model.
     */
    def editInstancesFactory:PageFactory
    
    /**
     * The page that lets you edit a given Model.
     */
    def modelDesignerFactory:PageFactory
    
    /**
     * Navigates to the Advanced Editor / Model Designer page.
     */
    def showAdvancedEditorFor(thingId:TID):Unit
    
    /**
     * Constructs a server-compatible "path" to identify a Property.
     */
    def propPath(propId:TID, thingIdOpt:Option[TID]):String
    def propPath(propId:TID):String
  }
}
