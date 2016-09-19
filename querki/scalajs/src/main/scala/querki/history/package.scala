package querki

import autowire.Core.Request

import querki.ecology._
import querki.globals._
import querki.history.HistoryFunctions._
import querki.pages.PageFactory

package object history {
  trait History extends EcologyInterface {
    def historySummaryFactory:PageFactory
    
    /**
     * True iff we are in "viewing history" mode.
     */
    def viewingHistory:Boolean
    
    /**
     * If we are viewing history, what version are we viewing?
     */
    def currentHistoryVersion:Option[HistoryVersion]
    
    def setHistoryVersion(v:HistoryVersion):Unit
    
    /**
     * Returns true iff the specified Request may be performed while viewing a previous
     * Version. Most operations are *not* legal!
     */
    def isLegalDuringHistory(req:Request[String]):Boolean
  }
}
