package querki.location

import querki.test._
import querki.types._

/**
 * @author jducoeur
 */
class LocationTests extends QuerkiTests {

  lazy val Location = interface[querki.location.Location]

  class TSpace extends CommonSpace {
    val locProp = new TestProperty(Location.LocationType, Optional, "My Location")

    val locThing = new SimpleTestThing(
      "With full Location",
      locProp(
        SimplePropertyBundle(
          Location.StreetAddressProp("100 Memorial Drive"),
          Location.TownProp("Cambridge"),
          Location.StateProp("MA")
        )
      )
    )

    val withoutState = new SimpleTestThing(
      "Without state",
      locProp(
        SimplePropertyBundle(
          Location.StreetAddressProp("100 Memorial Drive"),
          Location.TownProp("Cambridge")
        )
      )
    )

    val withoutAddr = new SimpleTestThing(
      "Without address",
      locProp(
        SimplePropertyBundle(
          Location.TownProp("Cambridge"),
          Location.StateProp("MA")
        )
      )
    )

    val emptyLoc = new SimpleTestThing(
      "Empty Location",
      locProp(
        SimplePropertyBundle()
      )
    )

    // Note that Google Maps doesn't actually like this address, but this is the specific example that triggered QI.bu6oe4i:
    val withUnicode = new SimpleTestThing(
      "With Unicode",
      locProp(
        SimplePropertyBundle(
          Location.StreetAddressProp("Arts at the Armory Caf\u00E9, 191 Highland Avenue"),
          Location.TownProp("Somerville"),
          Location.StateProp("MA")
        )
      )
    )
  }

  "Location text display" should {
    "show correctly, with a map, with all fields present" in {
      implicit val s = new TSpace

      pql("""[[With full Location -> My Location]]""") should
        equal(
          "100 Memorial Drive, Cambridge, MA ([map](https://www.google.com/maps/place/100+Memorial+Drive%2C+Cambridge%2C+MA))"
        )
    }

    "degrade gracefully if the state is missing" in {
      implicit val s = new TSpace

      pql("""[[Without state -> My Location]]""") should
        equal("100 Memorial Drive, Cambridge")
    }

    "degrade gracefully if the street address is missing" in {
      implicit val s = new TSpace

      pql("""[[Without address -> My Location]]""") should
        equal("Cambridge, MA")
    }

    "show a properly empty text if the Location is empty" in {
      implicit val s = new TSpace

      pql("""[[Empty Location -> My Location]]""") should
        equal("")
    }
  }

  "_mapLink" should {
    "display if all fields are present" in {
      implicit val s = new TSpace

      pql("""[[With full Location -> My Location -> _mapLink]]""") should
        equal(
          "[https://www.google.com/maps/place/100+Memorial+Drive%2C+Cambridge%2C+MA](https://www.google.com/maps/place/100+Memorial+Drive%2C+Cambridge%2C+MA)"
        )
    }

    "not display if the state is missing" in {
      implicit val s = new TSpace

      pql("""[[Without state -> My Location -> _mapLink]]""") should
        equal("")
    }

    "display with text" in {
      implicit val s = new TSpace

      pql("""[[With full Location -> My Location -> _mapLink -> ""__map__""]]""") should
        equal("[map](https://www.google.com/maps/place/100+Memorial+Drive%2C+Cambridge%2C+MA)")
    }

    // QI.bu6oe4i
    "work with Unicode in the name" in {
      implicit val s = new TSpace

      pql("""[[With Unicode -> My Location -> _mapLink]]""") should
        equal(
          "[https://www.google.com/maps/place/Arts+at+the+Armory+Caf%C3%A9%2C+191+Highland+Avenue%2C+Somerville%2C+MA](https://www.google.com/maps/place/Arts+at+the+Armory+Caf%C3%A9%2C+191+Highland+Avenue%2C+Somerville%2C+MA)"
        )
    }
  }
}
