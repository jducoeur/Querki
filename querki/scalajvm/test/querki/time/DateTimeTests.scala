package querki.time

import querki.ecology._

import querki.time._

import querki.test._

class DateTimeTests extends QuerkiTests {
  lazy val Time = interface[querki.time.Time]

  "DateTime" should {

    class TSpace extends CommonSpace {
      val dateProp = new TestProperty(Time.QDateTime, ExactlyOne, "DateTime Prop")

      val theModel = new SimpleTestThing("DateTime Model")
      val thing1 = new TestThing("Thing 1", theModel, dateProp(new DateTime(2013, 3, 15, 10, 30)))
      val thing2 = new TestThing("Thing 2", theModel, dateProp(new DateTime(2013, 4, 15, 10, 30)))
      val thing3 = new TestThing("Thing 3", theModel, dateProp(new DateTime(2013, 2, 15, 10, 30)))
      val thing4 = new TestThing("Thing 4", theModel, dateProp(new DateTime(2013, 3, 15, 10, 29)))
      val thing5 = new TestThing("Thing 5", theModel, dateProp(new DateTime(2013, 3, 16, 10, 30)))
    }

    "wikify in default format" in {
      val space = new TSpace

      processQText(thingAsContext[TSpace](space, _.thing1), """[[DateTime Prop]]""") should
        equal("""Mar 15, 2013 10:30:00 AM""")
    }

    "wikify in custom format" in {
      val space = new TSpace

      processQText(thingAsContext[TSpace](space, _.thing1), """[[DateTime Prop -> ""__MMM dd, hh:mm a__""]]""") should
        equal("""Mar 15, 10:30 AM""")
    }

    "sort correctly" in {
      implicit val s = new TSpace

      pql("""[[DateTime Model._instances -> _sort(DateTime Prop)]]""") should
        equal(listOfLinkText(s.thing3, s.thing4, s.thing1, s.thing5, s.thing2))
    }
  }

  "Date" should {
    class TSpace extends CommonSpace {
      val dateProp = new TestProperty(Time.QDate, ExactlyOne, "Date Prop")

      val theModel = new SimpleTestThing("Date Model")
      val thing1 = new TestThing("Thing 1", theModel, dateProp(new DateTime(2013, 3, 15, 10, 30)))
      val thing2 = new TestThing("Thing 2", theModel, dateProp(new DateTime(2013, 4, 15, 10, 30)))
      val thing3 = new TestThing("Thing 3", theModel, dateProp(new DateTime(2013, 2, 15, 10, 30)))
      val thing4 = new TestThing("Thing 4", theModel, dateProp(new DateTime(2013, 3, 15, 10, 29)))
      val thing5 = new TestThing("Thing 5", theModel, dateProp(new DateTime(2013, 3, 16, 10, 30)))
    }

    "extract the elements of the Date" in {
      implicit val s = new TSpace

      pql("[[Thing 1 -> Date Prop -> _year]]") should equal("2013")
      pql("[[Thing 1 -> Date Prop -> _month]]") should equal("3")
      pql("[[Thing 1 -> Date Prop -> _dayOfMonth]]") should equal("15")
    }
  }

  "Date and DateTime" should {
    class TSpace extends CommonSpace {
      val sortProp = new TestProperty(Basic.QLType, ExactlyOne, "Sort Function")

      val dateTimeProp = new TestProperty(Time.QDateTime, ExactlyOne, "DateTime Prop")
      val dateTimeModel = new SimpleTestThing("DateTime Model", sortProp("DateTime Prop"))
      val thing1 = new TestThing("Thing 1", dateTimeModel, dateTimeProp(new DateTime(2013, 3, 15, 10, 30)))
      val thing2 = new TestThing("Thing 2", dateTimeModel, dateTimeProp(new DateTime(2013, 4, 15, 10, 30)))
      val thing3 = new TestThing("Thing 3", dateTimeModel, dateTimeProp(new DateTime(2013, 2, 15, 10, 30)))
      val thing4 = new TestThing("Thing 4", dateTimeModel, dateTimeProp(new DateTime(2013, 3, 15, 10, 29)))
      val thing5 = new TestThing("Thing 5", dateTimeModel, dateTimeProp(new DateTime(2013, 3, 16, 10, 30)))

      val dateProp = new TestProperty(Time.QDate, ExactlyOne, "Date Prop")
      val dateModel = new SimpleTestThing("Date Model", sortProp("Date Prop"))
      val thing1a = new TestThing("Thing 1a", dateModel, dateProp(new DateTime(2016, 3, 15, 10, 30)))
      val thing2a = new TestThing("Thing 2a", dateModel, dateProp(new DateTime(2012, 4, 15, 10, 30)))
      val thing3a = new TestThing("Thing 3a", dateModel, dateProp(new DateTime(2014, 2, 15, 10, 30)))
      val thing4a = new TestThing("Thing 4a", dateModel, dateProp(new DateTime(2015, 3, 15, 10, 29)))
      val thing5a = new TestThing("Thing 5a", dateModel, dateProp(new DateTime(2011, 3, 16, 10, 30)))
    }

    "sort correctly" in {
      implicit val s = new TSpace

      pql("""[[<DateTime Model._instances, Date Model._instances> -> _sort(Sort Function)]]""") should
        equal(listOfLinkText(
          s.thing5a,
          s.thing2a,
          s.thing3,
          s.thing4,
          s.thing1,
          s.thing5,
          s.thing2,
          s.thing3a,
          s.thing4a,
          s.thing1a
        ))
    }

    "work with _plus and _minus" in {
      implicit val s = new TSpace

      pql("""[[Thing 1a -> Date Prop -> _plus(Duration Type(5, days))]]""") should
        equal("03/20/2016")
      pql("""[[Thing 1 -> DateTime Prop -> _plus(Duration Type(5, days))]]""") should
        equal("Mar 20, 2013 10:30:00 AM")

      pql("""[[Thing 1a -> Date Prop -> _minus(Duration Type(5, days))]]""") should
        equal("03/10/2016")
      pql("""[[Thing 1 -> DateTime Prop -> _minus(Duration Type(5, days))]]""") should
        equal("Mar 10, 2013 10:30:00 AM")
    }
  }
}
