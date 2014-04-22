package querki.uservalues

import models.PType

import querki.ecology._

object MOIDs extends EcotIds(44) {
  val RatingTypeOID = moid(1)
}

class UserValueEcot(e:Ecology) extends QuerkiEcot(e) with UserValues {
  import MOIDs._
  
  def getUserType(pt:PType[_]):Option[PType[_]] = {
    // This really ought to work with a simple match, but it doesn't. Why not? I am mystified...
    if (pt.isInstanceOf[TUserValue])
      Some(pt.asInstanceOf[TUserValue].userType)
    else
      None
  }
      
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