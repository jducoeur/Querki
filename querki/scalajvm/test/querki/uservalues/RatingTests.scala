package querki.uservalues

import models.PType

import querki.ecology._
import querki.test._

class RatingTests extends QuerkiTests {

  lazy val Ratings = interface[Ratings]

  class TSpace extends CommonSpace {
    val ratingThing = new SimpleTestThing("Rating Thing", Ratings.RatingProperty())
  }

  "The Ratings Property" should {
    // Unit test for Issue .3y2876t, which caused Rating._edit to crash:
    "be editable" in {
      implicit val s = new TSpace
      implicit val u = s.owner

      pql("""[[Rating Thing -> Rating._edit]]""") should
        include("data-labels='Poor,Fair,Good,Great,Excellent'")
    }
  }
}
