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
        equal ("""Mar 15, 2013 10:30:00 AM""")      
    }
    
    "wikify in custom format" in {
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.thing1), """[[DateTime Prop -> ""__MMM dd, hh:mm a__""]]""") should 
        equal ("""Mar 15, 10:30 AM""")      
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
    
    "show the year correctly" in {
      implicit val s = new TSpace
      
      pql("[[Thing 1 -> Date Prop -> _year]]") should equal ("2013")
    }
  }
}