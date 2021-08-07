package querki

import autowire.Core.Request

import querki.ecology._
import querki.globals._
import querki.history.HistoryFunctions.HistoryVersion
import querki.pages.PageFactory
import querki.time.Common.Timestamp

package object history {

  trait History extends EcologyInterface {
    def historySummaryFactory: PageFactory

    /**
     * True iff we are in "viewing history" mode.
     */
    def viewingHistory: Boolean

    /**
     * If we are viewing history, what version are we viewing?
     */
    def currentHistoryVersion: Option[HistoryVersion]

    /**
     * The timestamp of the current history version.
     */
    def currentHistoryTime: Option[String]

    def setHistoryVersion(
      v: HistoryVersion,
      time: Timestamp
    ): Unit
    def clearHistoryVersion(): Unit

    /**
     * Returns true iff the specified Request may be performed while viewing a previous
     * Version. Most operations are *not* legal!
     */
    def isLegalDuringHistory(req: Request[String]): Boolean
  }
}
