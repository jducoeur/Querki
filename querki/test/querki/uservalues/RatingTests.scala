package querki.uservalues

import models.PType

import querki.ecology._
import querki.test._

class RatingTests extends QuerkiTests {
  lazy val UserValues = interface[querki.uservalues.UserValues]
  lazy val RatingType = UserValues.RatingType
  
  "RatingType" should {
    "be testable as TUserValue" in {
      assert(RatingType.isInstanceOf[TUserValue])
      val result = RatingType match {
        case uvt:TUserValue => true
        case _ => false
      }
      assert(result)
      
      val pt:PType[_] = RatingType
      val result2 = pt match {
        case uvt:TUserValue => true
        case _ => false
      }
      assert(result2)
    }
  }
}