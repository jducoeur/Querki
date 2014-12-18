package querki

import querki.globals._

import querki.pages.{PageFactory, ThingPageFactory}

package object editing {
  trait Editing extends EcologyInterface {
    /**
     * The page that lets you edit all of the instances of a given Model.
     */
    def editInstancesFactory:PageFactory
    
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
  }
}
