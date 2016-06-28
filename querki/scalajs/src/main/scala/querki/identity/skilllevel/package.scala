package querki.identity

import querki.ecology._

package object skilllevel {
  
  trait Complexity {
    def name:String
    def desc:String
  }
  
  trait SkillLevel extends EcologyInterface {
    /**
     * What level of Complexity has the user currently selected?
     * 
     * TBD: Note that this currently defaults to Easy if there isn't a User logged-in. Is this correct?
     * It deserves some examination.
     */
    def current:Complexity
    
    /**
     * Displays a dialog to change the current level.
     * 
     * NOTE that this may lead to reloading the page!
     */
    def changeSkillLevel():Unit
  }
}
