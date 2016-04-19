package querki.location

import querki.test._
import querki.types._

/**
 * @author jducoeur
 */
class LocationTests extends QuerkiTests {
  
  lazy val Location = interface[querki.location.Location]
  
  "_mapLink" should {
    class TSpace extends CommonSpace {
      val locProp = new TestProperty(Location.LocationType, Optional, "My Location")
      
      val locThing = new SimpleTestThing("With full Location",
        locProp(
          SimplePropertyBundle(
            Location.StreetAddressProp("100 Memorial Drive"),
            Location.TownProp("Cambridge"),
            Location.StateProp("MA"))))
      
      val withoutState = new SimpleTestThing("Without state",
        locProp(
          SimplePropertyBundle(
            Location.StreetAddressProp("100 Memorial Drive"),
            Location.TownProp("Cambridge"))))
    }
    
    "display if all fields are present" in {
      implicit val s = new TSpace
      
      pql("""[[With full Location -> My Location -> _mapLink]]""") should
        equal ("[https://www.google.com/maps/place/100+Memorial+Drive,+Cambridge,+MA](https://www.google.com/maps/place/100+Memorial+Drive,+Cambridge,+MA)")
    }
    
    "not display if the state is missing" in {
      implicit val s = new TSpace
      
      pql("""[[Without state -> My Location -> _mapLink]]""") should
        equal ("")      
    }
    
    "display with text" in {
      implicit val s = new TSpace
      
      pql("""[[With full Location -> My Location -> _mapLink -> ""__map__""]]""") should
        equal ("[map](https://www.google.com/maps/place/100+Memorial+Drive,+Cambridge,+MA)")
    }
  }
}
