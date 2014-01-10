package querki.time

import querki.ecology._

import querki.time._

import querki.test._

class DateTimeTests extends QuerkiTests {
  lazy val Time = interface[querki.time.Time]
  
  "DateTime" should {
    
    val jodaTime1 = new DateTime(2013, 3, 15, 10, 30)
    
    class TSpace extends CommonSpace {
      val dateProp = new TestProperty(Time.QDateTime, ExactlyOne, "DateTime Prop")
      
      val theModel = new SimpleTestThing("DateTime Model")
      val thing1 = new TestThing("Thing 1", theModel, dateProp(jodaTime1))
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
  }
}