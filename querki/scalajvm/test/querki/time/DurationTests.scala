package querki.time

import querki.globals._
import querki.test._
import querki.types.SimplePropertyBundle

/**
 * @author jducoeur
 */
class DurationTests extends QuerkiTests {
  lazy val QDuration = interface[querki.time.QDuration]
  lazy val Time = interface[querki.time.Time]

  "A duration" should {
    class TSpace extends CommonSpace {
      val dateProp = new TestProperty(Time.QDate, ExactlyOne, "DateTime Prop")

      val dateThing = new SimpleTestThing(
        "Timed Thing",
        dateProp(new DateTime(2013, 3, 15, 10, 30)),
        QDuration.DurationProp(
          SimplePropertyBundle(
            QDuration.DurationKindProp(QDuration.DurationMonths),
            QDuration.DurationQuantityProp(5)
          )
        )
      )
    }

    "print correctly" in {
      implicit val s = new TSpace

      pql("[[Timed Thing -> Duration]]") should
        equal("5 months")
    }

    "be addable to a Date" in {
      implicit val s = new TSpace

      pqlt(s.dateThing, """[[DateTime Prop -> _plus(Duration)]]""") should
        equal("08/15/2013")
    }

    // QI.7w4g8l4 -- this does double-duty to test the constructors for Date and Duration:
    "be constructable" in {
      implicit val s = commonSpace

      pql("""[[Date Type(""2/1/2010"") -> _plus(Duration Type(2, months))]]""") should
        equal("04/01/2010")
    }
  }
}
