package querki.typeclass

import querki.ecology._
import querki.globals._

object MOIDs extends EcotIds(69) {
  val WhoMethodOID = moid(1)
  val DateMethodOID = moid(2)
}

/**
 * This is mainly a container for miscellaneous typeclasses that don't have a better obvious location.
 * 
 * On principle, this should contains essentially nothing *but* typeclass signatures, so that many other
 * Ecots can depend upon it without dependency-loop risks.
 */
class TypeclassEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with Typeclasses  {
  import MOIDs._
  
  val GeneralTag = "General Functions"
  
  lazy val WhoMethod = new AbstractFunction(WhoMethodOID, Received,
    toProps(
      setName("_who"),
      Categories(GeneralTag),
      Summary("Fetch who did something"),
      Details("""    VALUE -> _who -> Person
        |Given some sort of event, this produces the Person who did it.
        |
        |Note that this is only defined for
        |certain sorts of Things; if you find a place where you think it should work but it doesn't, please
        |bring it to our attention -- thanks!""".stripMargin)))
  
  lazy val DateMethod = new AbstractFunction(DateMethodOID, Received,
    toProps(
      setName("_date"),
      Categories(GeneralTag),
      Summary("Says what date something happened"),
      Details("""    VALUE -> _date -> Date
        |Given some sort of event, this produces when it happened.
        |
        |Note that this is only defined for
        |certain sorts of Things; if you find a place where you think it should work but it doesn't, please
        |bring it to our attention -- thanks!""".stripMargin)))
  
  override lazy val props = Seq(
    WhoMethod,
    DateMethod
  )
}
