package querki.time

import models._

import querki.ecology._
import querki.globals._
import querki.types.{ModeledPropertyBundle, ModelTypeDefiner}
import querki.util.PublicException

object DurationMOIDs extends EcotIds(57) {
  val DurationKindModelOID = moid(1)
  val DurationYearsOID = moid(2)
  val DurationMonthsOID = moid(3)
  val DurationWeeksOID = moid(4)
  val DurationDaysOID = moid(5)
  // TODO: these values below Days aren't actually implemented yet, since there is nothing
  // useful to do with them. But I'm reserving OID space for them here, since they are likely
  // eventually:
  val DurationHoursOID = moid(6)
  val DurationMinutesOID = moid(7)
  val DurationSecondsOID = moid(8)
  
  val DurationTypeOID = moid(9)
  val DurationQuantityPropOID = moid(10)
  val DurationKindPropOID = moid(11)
  val DurationModelOID = moid(12)
  val DurationPropOID = moid(13)
}
  
/**
 * Definitions relating to the somewhat complex "Duration" Type.
 * 
 * @author jducoeur
 */
class DurationEcot(e:Ecology) extends QuerkiEcot(e) with ModelTypeDefiner with QDuration {
  
  import DurationMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  val Editor = initRequires[querki.editing.Editor]
  val Links = initRequires[querki.links.Links]
  
  def toPeriod(duration:ModeledPropertyBundle, state:SpaceState):Period = {
    val durationKind:OID = duration.getPropOpt(durationKindProp)(state).getOrElse(throw new Exception("Expected a Duration, but didn't get one!")).first
    val durationQuantity = duration.getPropOpt(durationQuantityProp)(state).getOrElse(throw new Exception("Expected a Duration, but didn't get one!")).first
    
    durationKind match {
      case durationYears.id => Period.years(durationQuantity)
      case durationMonths.id => Period.months(durationQuantity)
      case durationWeeks.id => Period.weeks(durationQuantity)
      case durationDays.id => Period.days(durationQuantity)
    }
  }
  
  lazy val durationKindModel = ThingState(DurationKindModelOID, systemOID, RootOID,
    toProps(
      setName("_durationKindModel"),
      Links.NoCreateThroughLinkProp(true),
      Links.ChoiceOrderProp(DurationYearsOID, DurationMonthsOID, DurationWeeksOID, DurationDaysOID),
      setInternal))
  lazy val durationYears = ThingState(DurationYearsOID, systemOID, durationKindModel, 
      toProps(setName("_durationYears"), Basic.DisplayNameProp("years")))
  lazy val durationMonths = ThingState(DurationMonthsOID, systemOID, durationKindModel, 
      toProps(setName("_durationMonths"), Basic.DisplayNameProp("months")))
  lazy val durationWeeks = ThingState(DurationWeeksOID, systemOID, durationKindModel, 
      toProps(setName("_durationWeeks"), Basic.DisplayNameProp("weeks")))
  lazy val durationDays = ThingState(DurationDaysOID, systemOID, durationKindModel, 
      toProps(setName("_durationDays"), Basic.DisplayNameProp("days")))
      
  lazy val durationKindProp = new SystemProperty(DurationKindPropOID, LinkType, ExactlyOne,
    toProps(
      setName("_durationKindProp"),
      Editor.PromptProp("Kind"),
      setInternal,
      Links.LinkModelProp(durationKindModel),
      Editor.EditWidthProp(2)))
  
  lazy val durationQuantityProp = new SystemProperty(DurationQuantityPropOID, IntType, ExactlyOne,
    toProps(
      setName("_durationQuantityProp"),
      Editor.PromptProp("Quantity"),
      setInternal))
  
  lazy val durationModel = ThingState(DurationModelOID, systemOID, RootOID,
    toProps(
      setName("_durationModel"),
      setInternal,
      durationQuantityProp(1),
      durationKindProp(durationYears),
      Editor.InstanceProps(durationQuantityProp, durationKindProp),
      Basic.DisplayTextProp("[[_durationQuantityProp]] [[_durationKindProp -> Display Name]]")))
      
  lazy val DurationType = new ModelType(DurationTypeOID, durationModel, 
    toProps(
      setName("Duration Type"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Represents a length of time"),
      Details("""This type is available in case you want to build your own Duration properties. In
        |most cases, though, you should just use the built-in Duration Property, which is good enough
        |for most purposes.""".stripMargin)))
  
  lazy val durationProp = new SystemProperty(DurationPropOID, DurationType, ExactlyOne,
    toProps(
      setName("Duration"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Lets you define a length of time"),
      Details("""This property lets you choose years, months, weeks or days, and how many of them.
        |You can then add a Duration to a Date using _add, to get another Date.
        |
        |We might later extend Duration to come with hours, minutes and seconds, but that's not useful
        |until Querki has fancier timestamps. If you have a reason to want this, please drop us a note!""".stripMargin)))
      
  override lazy val things =
    Seq(
      durationKindModel,
      durationYears,
      durationMonths,
      durationWeeks,
      durationDays,
      durationModel
    )
    
  override lazy val props =
    Seq(
      durationKindProp,
      durationQuantityProp,
      durationProp
    )
    
  override lazy val types =
    Seq(
      DurationType
    )
}