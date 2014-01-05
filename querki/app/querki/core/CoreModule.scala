package querki.core

import models._
import models.Thing._

import models.system.OIDs.{systemOID, DisplayTextOID, InternalPropOID}
import models.system.{UrThing}
import models.system.{ExactlyOne, Optional}
import models.system.{LargeTextType, TextType, YesNoType}

import querki.conventions
import querki.ecology._

import modules.Module

class CoreModule(e:Ecology, val moduleId:Short) extends Module(e) {
  import MOIDs._

  /***********************************************
   * PROPERTIES
   ***********************************************/

  /**
   * The root Property, from which all others derive.
   */
  lazy val UrProp = Property(UrPropOID, systemOID, UrThing, TextType, ExactlyOne,
      toProps(
        setName("Property"),
        (InternalPropOID -> ExactlyOne(YesNoType(true))),
        (conventions.MOIDs.PropSummaryOID -> Optional(TextType("The root Property, from which all others derive."))),
        (DisplayTextOID -> Optional(LargeTextType("""[[Summary -> ""**____** -- ""]]
            |[[_if(Property Type -> _is(Internal Method Type), 
            |  ""**METHOD**"",
            |  ""Collection: [[Property Collection]] Type: [[Property Type]]"")]]
            |
            |
            |[[Details]]""".stripMargin)))
        ), modules.time.TimeModule.epoch)

  override lazy val props = Seq(
    UrProp
  )
}
