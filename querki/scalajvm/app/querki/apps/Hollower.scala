package querki.apps

import akka.actor._

import org.querki.requester._

import models._

import querki.globals._
import querki.identity.User
import querki.spaces.SpacePure
import querki.time.DateTime
import querki.types.ModelTypeDefiner

/**
 * The part of the Extract App process that "hollows out" the extracted items from the original Space.
 * This is run after we've actually created the new App: it goes through all the extracted Things,
 * points them at their App representations, and removes the duplicate Property Values.
 *
 * This is separated out solely to keep file sizes under control. Conceptually, it's part of ExtractAppActor.
 *
 * @author jducoeur
 */
private[apps] trait Hollower extends EcologyMember with SpacePure {

  private lazy val Apps = interface[Apps]
  private lazy val System = interface[querki.system.System]
  private lazy val Types = interface[querki.types.Types]

  private lazy val SystemSpace = System.State

  def hollowSpace(
    extractees: Extractees,
    childState: SpaceState,
    appState: SpaceState,
    idMap: Map[OID, OID]
  ): SpaceState = {
    // TODO: this is duplicating logic in AppRemapper. Can/should we merge them?
    val (models, instances) = extractees.state.things.values.partition(_.isModel(extractees.state))
    val (pages, plainInstances) = instances.partition(_.model == querki.basic.MOIDs.SimpleThingOID)

    val withoutInstances = deleteInstances(plainInstances, childState)
    val hollowedModels = hollowThings(models ++ pages, withoutInstances, idMap)
    val shadowed = markShadows(extractees.state.spaceProps.values ++ extractees.state.types.values, hollowedModels)
    val hollowedSpace =
      if (extractees.extractState)
        hollowThings(Seq(shadowed), shadowed, idMap)
      else
        shadowed

    hollowedSpace
  }

  /**
   * This removes most of the extracted instances from the child Space. We do *not* shadow Instances
   * unless they are Pages.
   *
   * Yes, this seems a bit arbitrary, but seems to match the best semantics. We don't want to shadow
   * enum values, for example, but we *do* want to shadow Pages, so that you can edit them.
   *
   * This may need further refinement as we go.
   */
  def deleteInstances(
    instances: Iterable[ThingState],
    childState: SpaceState
  ): SpaceState = {
    (childState /: instances) { (curState, instance) =>
      deletePure(instance.id, instance)(curState)
    }
  }

  /**
   * For each of the extracted Models and Pages, hollow out the original version, give it the model in the App,
   * and mark it as a Shadow.
   */
  def hollowThings(
    things: Iterable[Thing],
    childState: SpaceState,
    idMap: Map[OID, OID]
  ): SpaceState = {
    (childState /: things) { (curState, thing) =>
      modifyPure(
        thing.id,
        thing,
        idMap.get(thing),
        hollowThing(thing),
        true,
        DateTime.now
      )(curState)
    }
  }

  /**
   * The only Props we do *not* filter are the ones that have the NotInherited flag.
   *
   * We precalculate this, to keep the hollowing process as fast as possible.
   *
   * Note the explicit assumption that NotInherited only applies to System properties,
   * and that we should inherit everything *except* those Properties.
   */
  val uninheritedProps =
    SystemSpace
      .spaceProps
      .filter { pair =>
        val (k, prop) = pair
        prop.props.contains(querki.core.MOIDs.NotInheritedOID)
      }
      .keys
      .toSet

  lazy val propsToRetain: Set[OID] = Set(
    Types.ModelForTypeProp.id
  )

  def hollowThing(thing: Thing): PropMap = {
    // Remove all props on this Thing *except* the ones that are unique to it, and mark it as
    // a Shadow:
    thing.props.filterKeys(pid => uninheritedProps.contains(pid) || propsToRetain.contains(pid)) + Apps.ShadowFlag(true)
  }

  /**
   * Mark all of the extracted Props and Types as Shadows, but otherwise leave them intact.
   */
  def markShadows(
    extracted: Iterable[Thing],
    childState: SpaceState
  ): SpaceState = {
    (childState /: extracted) { (curState, t) =>
      modifyPure(t.id, t, None, t.props + Apps.ShadowFlag(true), true, t.modTime)(curState)
    }
  }
}
