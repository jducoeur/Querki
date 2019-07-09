package querki.experiments

import models.Kind
import querki.api.{ExperimentBadName, SpaceApiImpl, AutowireParams, ExperimentAlreadyExists}
import querki.core.NameUtils
import querki.experiments.ExperimentFunctions._
import querki.data.TID
import querki.globals._
import querki.spaces.messages.{ThingFound, ThingError, CreateThing}

import scala.concurrent.Future

class ExperimentFunctionsImpl(info:AutowireParams)(implicit e:Ecology)
  extends SpaceApiImpl(info, e) with ExperimentFunctions
{
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val Core = interface[querki.core.Core]

  def doRoute(req:Request):Future[String] = route[ExperimentFunctions](this)(req)

  /**
    * Creating an Experiment is really just a fancy way of creating an object, at least currently. But we put
    * more formality around it, to prevent too much potential havoc.
    */
  def createExperiment(name: String, description: String): Future[ExperimentInfo] = {
    // TODO: check whether this user is allowed to Experiment, and return ExperimentingNotAllowed if not.

    val linkName = NameUtils.canonicalize(name)
    if (linkName.length < MinExperimentNameLen) {
      Future.failed(ExperimentBadName(name))
    } else if (state.anythingByName(linkName).isDefined) {
      Future.failed(ExperimentAlreadyExists(name))
    } else {
      val createMsg = CreateThing(user, state.id, Kind.Thing, MOIDs.ExperimentModelOID,
        models.toProps(
          Core.setName(linkName),
          Basic.DisplayNameProp(name),
          Conventions.PropDetails(description)
        ))
      spaceRouter.request(createMsg).map { result =>
        result match {
          case ThingFound(id, newState) => {
            ExperimentInfo(
              id.toTID,
              Some(linkName),
              name,
              description
            )
          }
          case ThingError(ex, _) => {
            // It's already a PublicException, so throw it into the result:
            throw ex
          }
        }
      }
    }
  }

  def workOnExperiment(experimentId: TID): Future[Unit] = ???

  def stopExperimenting(): Future[Unit] = ???

  def dropExperiment(experimentId: TID): Future[Unit] = ???

  def mergeExperiment(experimentId: TID): Future[Unit] = ???
}
