package querki.uservalues

import querki.ecology._

object MOIDs extends EcotIds(44) {
  val RatingTypeOID = moid(1)
}

class UserValueEcot(e:Ecology) extends QuerkiEcot(e) {
  import MOIDs._
      
  /******************************************
   * TYPES
   ******************************************/
  
  class RatingType extends UserValueType[Int,DiscreteSummary[Int]](RatingTypeOID,
      toProps(
        setName("Rating Type")))
  {
    // TODO: is this really IntType, or is it a specialized subType? At the least, it needs to set
    // appropriate bounds on the values, and control the UI.
    val userType = Core.IntType
    
    val summarizer = new DiscreteSummarizer(userType)
  }
  lazy val RatingType = new RatingType
  
  override lazy val types = Seq(
    RatingType
  )
}