package querki.experiments

import querki.data.{TID, BasicThingInfo}

import scala.concurrent.Future

trait ExperimentFunctions {
  import ExperimentFunctions._

  /**
    * Create a new Experiment.
    *
    * This will return either the info about the new Experiment, or throw an ExperimentException.
    */
  def createExperiment(name: String, description: String): Future[ExperimentInfo]

  /**
    * Switch to working on the specified Experiment.
    */
  def workOnExperiment(experimentId: TID): Future[Unit]

  /**
    * Leave Experiment Mode, and go back to normal.
    */
  def stopExperimenting(): Future[Unit]

  /**
    * Give up on this Experiment.
    *
    * This should require appropriate permissions, and confirmation from the user before calling this.
    */
  def dropExperiment(experimentId: TID): Future[Unit]

  /**
    * Merge the given Experiment to the main Space.
    *
    * This should require appropriate permissions, and confirmation from the user before calling this.
    */
  def mergeExperiment(experimentId: TID): Future[Unit]
}

object ExperimentFunctions {
  final val MinExperimentNameLen = 5

  case class ExperimentInfo(
    oid: TID,
    linkName: Option[String],
    displayName: String,
    description: String) extends BasicThingInfo
}
