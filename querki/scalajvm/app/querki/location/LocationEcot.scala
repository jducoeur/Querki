package querki.location

import models.{PropertyBundle, ThingState, Wikitext}

import querki.core.QLText
import querki.ecology._
import querki.globals._
import querki.ql.InvocationValue
import querki.types.{ModelTypeDefiner, SimplePropertyBundle}

object MOIDs extends EcotIds(63) {
  val StreetAddressOID = moid(1)
  val TownOID = moid(2)
  val StateOID = moid(3)
  val LocationModelOID = moid(4)
  val LocationTypeOID = moid(5)
  val MapLinkOID = moid(6)
}

/**
 * @author jducoeur
 */
class LocationEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with ModelTypeDefiner with Location {
  
  import MOIDs._
  
  val Editing = initRequires[querki.editing.Editor]
  val Links = initRequires[querki.links.Links]
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val QL = interface[querki.ql.QL]
  
  val LocationTag = "Locations"
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val StreetAddressProp = new SystemProperty(StreetAddressOID, TextType, Optional,
    toProps(
      setName("_Location Street Address"),
      Editing.PromptProp("Street Address"),
      Editing.EditWidthProp(6),
      Categories(LocationTag),
      setInternal))
  
  lazy val TownProp = new SystemProperty(TownOID, TextType, Optional,
    toProps(
      setName("_Location Town"),
      Editing.PromptProp("Town"),
      Editing.EditWidthProp(3),
      Categories(LocationTag),
      setInternal))
  
  lazy val StateProp = new SystemProperty(StateOID, TextType, Optional,
    toProps(
      setName("_Location State"),
      Editing.PromptProp("State"),
      Editing.EditWidthProp(1),
      Categories(LocationTag),
      setInternal))

  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val LocationModel = ThingState(LocationModelOID, systemOID, RootOID,
    toProps(
      setName("_Location Model"),
      setInternal,
      Categories(LocationTag),
      Summary("The Model for the Location Type"),
      StreetAddressProp(),
      TownProp(),
      StateProp(),
      Editing.InstanceProps(StreetAddressProp, TownProp, StateProp),
      Basic.DisplayTextProp("""[[<_Location Street Address, _Location Town, _Location State> -> _join("", "")]][[_mapLink -> "" (__map__)""]]""".stripMargin)))
      
  override lazy val things = Seq(
    LocationModel
  )
  
  lazy val LocationType = new ModelType(LocationTypeOID, LocationModelOID,
    toProps(
      setName("Location Type"),
      Categories(LocationTag),
      // Show this in the Editor, despite it being a Model Type:
      Basic.ExplicitProp(true),
      Summary("A geographic location"),
      Details("""Create a Property of this Type if you want to represent a place.
        |
        |The main advantage of using a Location instead of a simple text field is that, if you
        |fill in the full address, it will automatically provide a link to a Google Map for that
        |address.
        |
        |For the moment, Location Type only copes with street addresses in the US. This will be
        |enhanced in the future, to let you set the Location to "here" (if you are on a smartphone),
        |and to cope with non-US addresses. If you need these capabilities (or others), please speak up
        |so we can prioritize them appropriately.""".stripMargin)))
  
  override lazy val types = Seq(
    LocationType
  )
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/

  lazy val MapLinkFunction = new InternalMethod(MapLinkOID,
    toProps(
      setName("_mapLink"),
      Categories(LocationTag),
      Summary("Get a link to the Google Map for a Location"),
      Signature(
        expected = Some((Seq(LocationType), "A Location")),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (Links.URLType, "The Google Maps link for this Location, if possible")
      ),
      Details("""**If** this Location is sufficiently filled-in, the `_mapLink` function will produce a link
                |to the Google Map for this Location. This will currently only work if all fields are filled in;
                |otherwise, `_mapLink` will produce Nothing.
                |
                |Note the "garbage in, garbage out" principle: this link is only as good as the filled-in address.
                |If Google can't figure out where it's talking about, the map likely won't be useful.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      implicit val s = inv.state
      
      def txt(bundle:PropertyBundle, prop:Property[QLText,_]):InvocationValue[String] = {
        for {
          text <- inv.opt(bundle.getPropAll(prop).headOption)
          wiki <- inv.fut(QL.process(text, inv.context, Some(inv)))
        }
          yield wiki.strip.toString
      }
       
      for {
        locationBundle <- inv.contextAllAs(LocationType)
        addr <- txt(locationBundle, StreetAddressProp)
        town <- txt(locationBundle, TownProp)
        state <- txt(locationBundle, StateProp)
        str = s"$addr, $town, $state".replace(" ", "+")
      }
        yield ExactlyOne(Links.URLType(s"https://www.google.com/maps/place/" + str))
    }
  }
  
  override lazy val props = Seq(
    StreetAddressProp,
    TownProp,
    StateProp,
    
    MapLinkFunction
  )
}
