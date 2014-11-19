package querki

import querki.globals._

import querki.pages.PageFactory

package object editing {
  trait Editing extends EcologyInterface {
    /**
     * The page that lets you edit all of the instances of a given Model.
     */
    def editInstancesFactory:PageFactory
  }
}