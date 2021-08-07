package querki.spaces

import models.Kind.Kind
import models.{Kind, ModelPersistence, OID, ThingState}
import querki.basic.Basic
import querki.ecology.{EcologyInterface, QuerkiEcot}
import querki.globals.{spew, Ecology, SpaceState}
import querki.identity.{Person, User}
import querki.publication.{CurrentPublicationState, Publication}
import querki.security.AccessControl
import querki.spaces.SpaceMessagePersistence._
import querki.spaces.messages.CurrentState
import querki.system.System
import querki.uservalues.PersistMessages.OneUserValue
import querki.util.QLog

/**
 * This is a home for pure functions that take a Space and some sort of events, and return an updated Space.
 */
trait SpaceEvolution extends SpacePure with ModelPersistence {

  /**
   * This combines the various streams of information about a Space, and figures out how to handle it for this
   * specific user.
   *
   * TODO: update more efficiently, based on the given changes. Only do a full recompute for edge cases!
   *
   * @param oldState the previous filtered state for this User
   * @param user who we are filtering for
   * @param userValues the UserValues for this User
   * @param pubState the current state of Publications, if any
   * @param changes a new event that is changing the state of this Space
   *
   * @return the new state, appropriately filtered for this user
   */
  def updateForUser(
    oldState: Option[SpaceState]
  )(
    user: User
  )(
    fullState: SpaceState,
    events: List[SpaceEvent]
  )(implicit
    ecology: Ecology
  ): SpaceState = {
    val AccessControl = ecology.api[AccessControl]

    // Managers act as Owners for purposes of being able to read everything:
    val isManager = AccessControl.isManager(user, fullState)
    if (isManager) {
      fullState
    } else {
      val readFiltered =
        oldState.flatMap(s => evolveForEvents(s, user, fullState, events, AccessControl))
          .getOrElse(filterFully(user, fullState, AccessControl))

      if (AccessControl.canRead(readFiltered, user, fullState.id))
        readFiltered
      else {
        // This user isn't allowed to read the Root Page, so give them a stub instead.
        // TODO: this is a fairly stupid hack, but we have to be careful -- filtering out
        // bits of the Space while not breaking it entirely is tricky. Think about how to
        // do this better.
        readFiltered.copy(pf = readFiltered.props +
          (ecology.api[Basic].DisplayTextProp("**You aren't allowed to read this Page; sorry.**")))
      }
    }
  }

  /**
   * If we can evolve the given state changes intelligently, do so.
   *
   * This is basically how we avoid having to do the fairly evil [[filterFully()]] every time there is a change.
   * Most world changes are simple, and require only tiny tweaks to the existing filtered state.
   *
   * TODO: in principle, we can make this even more efficient by scanning the events and pre-computing whether or not
   * efficient evolution will be possible before we bother doing it. (Because sometimes we're going to evolve for a
   * while and then hit an event that foils us and forces a full filter.) But in practice, getting this right is
   * tricky (since later events depend on earlier ones), so we're not going to worry about that yet.
   */
  private def evolveForEvents(
    oldState: SpaceState,
    user: User,
    fullState: SpaceState,
    events: List[SpaceEvent],
    AccessControl: AccessControl
  ): Option[SpaceState] = {
    // Run through all of the events in changes. If all of the evolutions produce Some, we win:
    (Option(oldState) /: events) { (curStateOpt, event) =>
      curStateOpt.flatMap(curState => evolveForEvent(curState, user, fullState, event, AccessControl))
    }
  }

  /**
   * Given a single event, evolve for that if possible.
   */
  private def evolveForEvent(
    prevState: SpaceState,
    user: User,
    fullState: SpaceState,
    event: SpaceEvent,
    AccessControl: AccessControl
  ): Option[SpaceState] = {
    // Needed for some implicit conversions:
    implicit val s = fullState
    event match {
      case DHCreateThing(requester, oid, kind, modelId, props, modTime, _) => {
        def applyCreate(): Option[SpaceState] = {
          Some(createPure(requester, kind, oid, modelId, props, modTime)(prevState))
        }
        // TODO: Kind really ought to be an ADT, not just an Int, and we should match here:
        if (kind == Kind.Property) {
          // Properties are always public, so this is always legit:
          applyCreate()
        } else if (kind == Kind.Thing) {
          if (modelId == querki.security.MOIDs.InstancePermissionsModelOID) {
            // Instance Permission objects can affect downstream instance visibility, so for now we're going to just
            // re-filter instead of trying to be clever here:
            None
          } else if (AccessControl.canRead(fullState, user, oid)) {
            // The common case, probably accounting for 99% of Creates:
            applyCreate()
          } else {
            // If this person can't read it, this is a no-op:
            Some(prevState)
          }
        } else if (kind == Kind.Type) {
          // TODO: in principle, we ought to be able to deal with this if the Type Model is visible, right?
          None
        } else
          // We might deal with Collection someday, and Space is just weird:
          None
      }
      case DHModifyThing(req, id, modelIdOpt, propChanges, replaceAllProps, modTime) => {
        fullState.anything(id) match {
          case Some(thing) => {
            def applyModify(): Option[SpaceState] = {
              Some(modifyPure(id, thing, Some(thing.model), propChanges, replaceAllProps, modTime)(prevState))
            }

            // Weird special case: when someone is removed from this Space, they transition from member to non-member:
            def isRemovingThisCaller: Boolean = {
              if (thing.model == querki.identity.MOIDs.PersonOID) {
                val Person = ecology.api[querki.identity.Person]
                // It's a removal if they were defined in the previous version but aren't in the new one:
                Person.localPerson(user)(prevState).isDefined && Person.localPerson(user)(fullState).isEmpty
              } else
                false
            }

            val kind: Kind = thing.kind
            if (kind == Kind.Property) {
              // Same logic as in DHCreateThing:
              applyModify()
            } else if (kind == Kind.Thing) {
              // This is subtle, because the visibility of the Thing could have been changed. So we need to check both
              // before and after:
              val couldReadBefore: Boolean = prevState.anything(id).isDefined
              val canReadAfter: Boolean = AccessControl.canRead(fullState, user, id)
              if (thing.model == querki.security.MOIDs.InstancePermissionsModelOID) {
                // We modified an Instance Permissions Object. At least for now, we're not going to try to deal with
                // this efficiently, since it potentially affects the visibility of this Model's Instances, so we
                // might have to go add/remove instances from the view.
                None
              } else if (isRemovingThisCaller) {
                // This person has been removed from the Space:
                None
              } else if (
                (thing.model == querki.identity.MOIDs.PersonOID) &&
                (ecology.api[querki.identity.Person].localPerson(user).map(_.id == thing.id).getOrElse(false))
              ) {
                // We modified this User's local Person record, which might mean we've changed their Roles, so do a
                // full reload:
                None
              } else if (couldReadBefore && canReadAfter) {
                // Still visible, so do the modification:
                applyModify()
              } else if (!couldReadBefore && !canReadAfter) {
                // Wasn't and isn't visible, so this is a no-op
                Some(prevState)
              } else if (couldReadBefore && !canReadAfter) {
                // It's been removed from our view
                Some(deletePure(id, thing)(prevState))
              } else if (!couldReadBefore && canReadAfter) {
                // It has been added to our view
                Some(createPure(req, kind, id, thing.model, thing.props, modTime)(prevState))
              } else {
                // This shouldn't be possible:
                QLog.error(s"Logic error processing $event!")
                None
              }
            } else
              None
          }
          case None => {
            QLog.error(
              s"Found a DHModifyThing for unknown Thing $id in Space ${fullState.displayName} (${fullState.id})!"
            )
            None
          }
        }
      }

      case DHDeleteThing(req, id, modTime) => {
        // This one's easy: if we could see it before, delete it:
        prevState.anything(id) match {
          case Some(thing) => Some(deletePure(id, thing)(prevState))
          case None        => None
        }
      }

      // This is too complex to analyze at this time:
      case DHAddApp(req, modTime, appState, parentApps, shadowMap, afterExtraction) => None

      // Should be a non-issue -- the Space should be nearly empty:
      case DHInitState(req, display) => None

      // All bets are off: this is slamming a totally new state, so we need to recalculate from scratch:
      case DHSetState(state, modTime, reason, details) => None
    }
  }

  /**
   * When we can't do a smart evolution based on the event, filter the entire Space the hard way.
   */
  private def filterFully(
    user: User,
    rs: SpaceState,
    AccessControl: AccessControl
  )(implicit
    ecology: Ecology
  ): SpaceState = {
    // If we are unit-testing, notify the tests that we are doing a full evolution:
    ecology.api[querki.spaces.SpaceOps].notifyFullEvolution()
    // TODO: MAKE THIS MUCH FASTER! This is probably O(n**2), maybe worse. How can we make this better?
    (rs /: rs.things) { (curState, thingPair) =>
      val (thingId, thing) = thingPair
      // Note that we need to pass rs into canRead(), not curState. That is because curState can
      // be in an inconsistent state while we're in the middle of all this. For example, we may
      // have already exised a Model from curState (because we don't have permission) when we get
      // to an Instance of that Model. Things can then get horribly confused when we try to look
      // at the Instance, try to examine its Model, and *boom*.
      if (AccessControl.canRead(rs, user, thingId) || isReadException(thingId, user)(rs, ecology)) {
        // Remove any SystemHidden Properties from this Thing, if there are any:
        val hiddenOIDs = ecology.api[System].hiddenOIDs
        if (hiddenOIDs.exists(thing.props.contains(_))) {
          val newThing = thing.copy(pf = {
            (thing.props -- hiddenOIDs)
          })
          curState.copy(things = (curState.things + (newThing.id -> newThing)))
        } else
          curState
      } else
        curState.copy(things = (curState.things - thingId))
    }
  }

  def enhanceWithUserValues(
    state: SpaceState,
    userValues: Seq[OneUserValue]
  ): SpaceState = {
    (state /: userValues) { (curState, uv) =>
      if (uv.thingId == curState.id) {
        // We're enhancing the Space itself:
        curState.copy(pf = (curState.props + (uv.propId -> uv.v)))
      } else curState.anything(uv.thingId) match {
        case Some(thing: ThingState) => {
          val newThing = thing.copy(pf = thing.props + (uv.propId -> uv.v))
          curState.copy(things = curState.things + (newThing.id -> newThing))
        }
        // Yes, this looks like an error, but it isn't: it means that there was a UserValue
        // for a deleted Thing.
        case _ => curState
      }
    }
  }

  def enhanceWithPublication(
    state: SpaceState,
    pubState: Option[CurrentPublicationState]
  )(implicit
    ecology: Ecology
  ): SpaceState = {
    pubState.map { ps =>
      ecology.api[Publication].enhanceState(state, ps)
    }.getOrElse(state)
  }

  /**
   * This is the dumping ground for exceptions to the rule that your Space only contains Things you can
   * read. There should *not* be many of these.
   */
  def isReadException(
    thingId: OID,
    user: User
  )(implicit
    state: SpaceState,
    ecology: Ecology
  ): Boolean = {
    // I always have access to my own Person record, so that _me always works:
    ecology.api[Person].hasPerson(user, thingId)
  }
}
