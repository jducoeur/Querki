package querki.experiments

import models.ThingState
import querki.ecology.{QuerkiEcot, EcotIds}
import querki.globals.Ecology

object MOIDs extends EcotIds(76) {
  val ExperimentModelOID = moid(1)
  val ExperimentStatusModelOID = moid(2)
  val ExperimentActiveOID = moid(3)
  val ExperimentDroppedOID = moid(4)
  val ExperimentMergedOID = moid(5)
  val ExperimentStatusPropOID = moid(6)
}

class ExperimentsEcot(e: Ecology) extends QuerkiEcot(e) with Experiments {

  import MOIDs._

  val Links = initRequires[querki.links.Links]

  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]

  override def postInit() = {
    ApiRegistry.registerApiImplFor[ExperimentFunctions, ExperimentFunctionsImpl](SpaceOps.spaceRegion)
  }

  /******************************************
   * PROPERTIES
   ******************************************/

  lazy val ExperimentStatusProp = new SystemProperty(ExperimentStatusPropOID, LinkType, ExactlyOne,
    toProps(
      setName("_Experiment Status"),
      setInternal,
      Links.LinkModelProp(ExperimentStatusModel),
      Links.ChoiceOrderProp(ExperimentActiveOID, ExperimentMergedOID, ExperimentDroppedOID)
    ))

  /******************************************
   * THINGS
   ******************************************/

  lazy val ExperimentStatusModel = ThingState(ExperimentStatusModelOID, systemOID, RootOID,
    toProps(
      setName("_Experiment Status Model"),
      setInternal,
      Categories(ExperimentsTag)
    ))

  lazy val ExperimentActive = ThingState(ExperimentActiveOID, systemOID, ExperimentStatusModelOID,
    toProps(
      setName("_Experiment Active"),
      setInternal,
      Categories(ExperimentsTag)
    ))

  lazy val ExperimentDropped = ThingState(ExperimentDroppedOID, systemOID, ExperimentStatusModelOID,
    toProps(
      setName("_Experiment Dropped"),
      setInternal,
      Categories(ExperimentsTag)
    ))

  lazy val ExperimentMerged = ThingState(ExperimentMergedOID, systemOID, ExperimentStatusModelOID,
    toProps(
      setName("_Experiment Merged"),
      setInternal,
      Categories(ExperimentsTag)
    ))

  lazy val ExperimentModel = ThingState(ExperimentModelOID, systemOID, RootOID,
    toProps(
      setName("_Experiment Model"),
      setInternal,
      Categories(ExperimentsTag),
      // Experiments start out Active, so that's the default:
      ExperimentStatusProp(ExperimentActiveOID)
    ))
}
