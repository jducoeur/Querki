package querki.uservalues

import akka.actor.Props

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
  
  def wrapUserValue(uv:QValue, pt:PType[_], oldWrapperOpt:Option[QValue]):QValue = {
    if (pt.isInstanceOf[UserValueType[_,_]]) {
      pt.asInstanceOf[UserValueType[_,_]].wrapValue(uv, oldWrapperOpt)
    } else
      throw new Exception("UserValueEcot asked to wrap something in a PType that isn't UserValueType!")
  }
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def userValuePersisterProps(spaceId:OID):Props = 
    Props(new UserValuePersister(spaceId, ecology))
      
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