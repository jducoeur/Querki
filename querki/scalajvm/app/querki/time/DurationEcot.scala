package querki.time

import models._

import querki.ecology._
import querki.globals._
import querki.types.{ModelTypeDefiner, ModeledPropertyBundle, SimplePropertyBundle}
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
class DurationEcot(e: Ecology) extends QuerkiEcot(e) with ModelTypeDefiner with querki.core.MethodDefs with QDuration {

  import DurationMOIDs._

  val Basic = initRequires[querki.basic.Basic]
  val Editor = initRequires[querki.editing.Editor]
  val Links = initRequires[querki.links.Links]

  def toPeriod(
    duration: ModeledPropertyBundle,
    state: SpaceState
  ): Period = {
    val durationKind: OID = duration.getPropOpt(DurationKindProp)(state).getOrElse(throw new Exception(
      "Expected a Duration, but didn't get one!"
    )).first
    val durationQuantity = duration.getPropOpt(DurationQuantityProp)(state).getOrElse(throw new Exception(
      "Expected a Duration, but didn't get one!"
    )).first

    durationKind match {
      case DurationYears.id  => Period.years(durationQuantity)
      case DurationMonths.id => Period.months(durationQuantity)
      case DurationWeeks.id  => Period.weeks(durationQuantity)
      case DurationDays.id   => Period.days(durationQuantity)
    }
  }

  lazy val durationKindModel = ThingState(
    DurationKindModelOID,
    systemOID,
    RootOID,
    toProps(
      setName("_durationKindModel"),
      Links.NoCreateThroughLinkProp(true),
      Links.ChoiceOrderProp(DurationYearsOID, DurationMonthsOID, DurationWeeksOID, DurationDaysOID),
      setInternal
    )
  )

  lazy val DurationYears = ThingState(
    DurationYearsOID,
    systemOID,
    durationKindModel,
    toProps(setName("_durationYears"), Basic.DisplayNameProp("years"))
  )

  lazy val DurationMonths = ThingState(
    DurationMonthsOID,
    systemOID,
    durationKindModel,
    toProps(setName("_durationMonths"), Basic.DisplayNameProp("months"))
  )

  lazy val DurationWeeks = ThingState(
    DurationWeeksOID,
    systemOID,
    durationKindModel,
    toProps(setName("_durationWeeks"), Basic.DisplayNameProp("weeks"))
  )

  lazy val DurationDays = ThingState(
    DurationDaysOID,
    systemOID,
    durationKindModel,
    toProps(setName("_durationDays"), Basic.DisplayNameProp("days"))
  )

  lazy val DurationKindProp = new SystemProperty(
    DurationKindPropOID,
    LinkType,
    ExactlyOne,
    toProps(
      setName("_durationKindProp"),
      Editor.PromptProp("Kind"),
      setInternal,
      Links.LinkModelProp(durationKindModel),
      Editor.EditWidthProp(2)
    )
  )

  lazy val DurationQuantityProp = new SystemProperty(
    DurationQuantityPropOID,
    IntType,
    ExactlyOne,
    toProps(
      setName("_durationQuantityProp"),
      Editor.PromptProp("Quantity"),
      setInternal
    )
  )

  lazy val durationModel = ThingState(
    DurationModelOID,
    systemOID,
    RootOID,
    toProps(
      setName("_durationModel"),
      setInternal,
      DurationQuantityProp(1),
      DurationKindProp(DurationYears),
      Editor.InstanceProps(DurationQuantityProp, DurationKindProp),
      Basic.DisplayTextProp("[[_durationQuantityProp]] [[_durationKindProp -> Name]]")
    )
  )

  lazy val DurationType = new ModelType(
    DurationTypeOID,
    durationModel,
    toProps(
      setName("Duration Type"),
      SkillLevel(SkillLevelAdvanced),
      // List this Type even though it's based on a Model:
      Basic.ExplicitProp(true),
      Categories(TimeTag),
      Summary("Represents a length of time"),
      Details("""This type is available in case you want to build your own Duration properties. In
                |most cases, though, you should just use the built-in Duration Property, which is good enough
                |for most purposes.
                |
                |Occasionally, you may want to specify a literal Duration in a QL expression. You do this by
                |calling `Duration Type` as a function with two parameters: the number of units, and the unit.
                |So for example, to add two months to today's date, you would say:
                |```
                |_today -> _plus(Duration Type(2, months))
                |```
                |Note that those are two distinct parameters -- there must be a comma between them. Also, only
                |the plural forms of the units are allowed, so one month is still `Duration Type(1, months)`.
                |
                |The available units are "years", "months", "weeks" and "days". (Eventually we will also allow
                |hours, minutes and seconds, but those are not implemented yet.) """.stripMargin)
    )
  ) {

    override def constructTypeValue(inv: Invocation): Option[QFut] = {
      val result: QFut = for {
        n <- inv.processParamFirstAs(0, IntType)
        unitId <- inv.processParamFirstAs(1, LinkType)
        unit <- inv.opt(inv.state.anything(unitId), Some(PublicException("Duration.illegalUnit")))
        if (unit.isAncestor(durationKindModel)(inv.state))
        v = this(SimplePropertyBundle(DurationKindProp(unit), DurationQuantityProp(n)))
      } yield ExactlyOne(v)

      Some(result)
    }
  }

  lazy val DurationProp = new SystemProperty(
    DurationPropOID,
    DurationType,
    ExactlyOne,
    toProps(
      setName("Duration"),
      SkillLevel(SkillLevelAdvanced),
      Categories(TimeTag),
      Summary("Lets you define a length of time"),
      Details(
        """This property lets you choose years, months, weeks or days, and how many of them.
          |You can then add a Duration to a Date using _add, to get another Date.
          |
          |We might later extend Duration to come with hours, minutes and seconds, but that's not useful
          |until Querki has fancier timestamps. If you have a reason to want this, please drop us a note!""".stripMargin
      )
    )
  )

  override lazy val things =
    Seq(
      durationKindModel,
      DurationYears,
      DurationMonths,
      DurationWeeks,
      DurationDays,
      durationModel
    )

  override lazy val props =
    Seq(
      DurationKindProp,
      DurationQuantityProp,
      DurationProp
    )

  override lazy val types =
    Seq(
      DurationType
    )
}
