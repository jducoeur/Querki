package querki.uservalues

import querki.core.IntTypeBasis
import querki.ecology._

object RatingMOIDs extends EcotIds(45) {
  val RatingTypeOID = moid(1)
  val IntSummarizerOID = moid(2)  
}

class RatingEcot(e:Ecology) extends QuerkiEcot(e) with IntTypeBasis with SummarizerDefs {
  import RatingMOIDs._
      
  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val RatingType = new IntTypeBase(RatingTypeOID,
    toProps(
      setName("Rating Type")))
  
  lazy val IntSummarizer = new DiscreteSummarizer(IntSummarizerOID, Core.IntType,
    toProps(
      setName("Number Summarizer"),
      Summary("Given a User Value Property made of numbers (such as Ratings), this provides functions such as _average.")))

  override lazy val types = Seq(
    RatingType,
    IntSummarizer
  )
}
