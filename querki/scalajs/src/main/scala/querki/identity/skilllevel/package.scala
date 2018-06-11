package querki.identity

import querki.ecology._

package object skilllevel {
  
  trait Complexity {
    def name:String
    def desc:String
    def accepts(actual:Complexity):Boolean
  }
  
  case object UnspecifiedComplexity extends Complexity {
    val name = "Unspecified"
    val desc = ""
    def accepts(actual:Complexity) = true
  }
  
  trait SkillLevel extends EcologyInterface {
    
    def EasyComplexity:Complexity
    def StandardComplexity:Complexity
    def AdvancedComplexity:Complexity
    
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
    
    /**
     * Forced the SkillLevel to update to match the current User.
     */
    def updateSkillLevel(): Unit
  }
}
