package querki.uservalues

import scala.xml.NodeSeq

import models.DisplayPropVal

import querki.core.IntTypeBasis
import querki.ecology._
import querki.values.{ElemValue, RequestContext}

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
  {
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = 
      <div class='rating' data-rating={get(v).toString}></div>
  }
  
  lazy val IntSummarizer = new DiscreteSummarizer(IntSummarizerOID, Core.IntType,
    toProps(
      setName("Number Summarizer"),
      Summary("Given a User Value Property made of numbers (such as Ratings), this provides functions such as _average.")))

  override lazy val types = Seq(
    RatingType,
    IntSummarizer
  )
}
