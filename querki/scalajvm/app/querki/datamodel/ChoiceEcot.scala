package querki.datamodel

import models.Kind

import querki.ecology._
import querki.globals._

object ChoiceMOIDs extends EcotIds(73) {
  val ChooseFromPropertyOID = moid(1)
  val ChooseFromThingOID = moid(2)
  val ChooseFromThingThroughOID = moid(3)
}

class ChoiceEcot(e: Ecology) extends QuerkiEcot(e) with Choices {
  import ChoiceMOIDs._

  val Links = initRequires[querki.links.Links]

  /**
   * *********************************************
   * PROPERTIES
   * *********************************************
   */

  lazy val ChooseFromPropProp = new SystemProperty(
    ChooseFromPropertyOID,
    LinkType,
    Optional,
    toProps(
      setName("_chooseFromProperty"),
      SkillLevel(SkillLevelAdvanced),
      Core.AppliesToKindProp(Kind.Property),
      Links.LinkKindProp(Kind.Property),
      Categories(DataModelTag),
      Summary("""When a Property is a Choice, this says which other Property the choices are drawn from.""")
    )
  )

  lazy val ChooseFromThingProp = new SystemProperty(
    ChooseFromThingOID,
    LinkType,
    Optional,
    toProps(
      setName("_chooseFromThing"),
      SkillLevel(SkillLevelAdvanced),
      Core.AppliesToKindProp(Kind.Property),
      Categories(DataModelTag),
      Summary("""When a Property is a Choice, this says which Thing the choices can be found on.""")
    )
  )

  lazy val ChooseFromThingThroughProp = new SystemProperty(
    ChooseFromThingThroughOID,
    LinkType,
    Optional,
    toProps(
      setName("_chooseFromThingThrough"),
      SkillLevel(SkillLevelAdvanced),
      Core.AppliesToKindProp(Kind.Property),
      Links.LinkKindProp(Kind.Property),
      Categories(DataModelTag),
      Summary(
        """When a Property is a Choice, this can list *another* Property that specifies which Thing the choices come from."""
      )
    )
  )

  override lazy val props = Seq(
    ChooseFromPropProp,
    ChooseFromThingProp,
    ChooseFromThingThroughProp
  )
}
