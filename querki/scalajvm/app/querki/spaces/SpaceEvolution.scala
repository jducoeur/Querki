package querki.spaces

import models.{OID, ThingState}
import querki.basic.Basic
import querki.globals.{SpaceState, Ecology}
import querki.identity.{User, Person}
import querki.publication.{Publication, PublicationState, CurrentPublicationState}
import querki.spaces.messages.CurrentState
import querki.system.System
import querki.uservalues.PersistMessages.OneUserValue
import querki.values.SpaceState

/**
  * This is a home for pure functions that take a Space and some sort of events, and return an updated Space.
  */
object SpaceEvolution {
  /**
    * This combines the various streams of information about a Space, and figures out how to handle it for this
    * specific user.
    */
  def updateForUser(
    oldState: Option[SpaceState]
  )(
    user: User, userValues: Seq[OneUserValue], pubState: Option[CurrentPublicationState]
  )(
    changes: CurrentState
  )(implicit ecology: Ecology): SpaceState = {
    val AccessControl = ecology.api[querki.security.AccessControl]
    val rs = changes.state

    // Managers act as Owners for purposes of being able to read everything:
    val isManager = AccessControl.isManager(user, rs)
    val safeState =
      if (isManager) {
        rs
      } else {
        // TODO: MAKE THIS MUCH FASTER! This is probably O(n**2), maybe worse. We need to do heavy
        // caching, and do much more sensitive updating as things change.
        val readFiltered = (rs /: rs.things) { (curState, thingPair) =>
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
              val newThing = thing.copy(pf = { (thing.props -- hiddenOIDs) })
              curState.copy(things = (curState.things + (newThing.id -> newThing)))
            } else
              curState
          } else
            curState.copy(things = (curState.things - thingId))
        }

        if (AccessControl.canRead(readFiltered, user, rs.id))
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

    val stateWithUV = (safeState /: userValues) { (curState, uv) =>
      if (uv.thingId == curState.id) {
        // We're enhancing the Space itself:
        curState.copy(pf = (curState.props + (uv.propId -> uv.v)))
      }
      else curState.anything(uv.thingId) match {
        case Some(thing:ThingState) => {
          val newThing = thing.copy(pf = thing.props + (uv.propId -> uv.v))
          curState.copy(things = curState.things + (newThing.id -> newThing))
        }
        // Yes, this looks like an error, but it isn't: it means that there was a UserValue
        // for a deleted Thing.
        case _ => curState
      }
    }

    // If there is a PublicationState, overlay that on top of the rest:
    // TODO (QI.7w4g8n8): there is a known bug here, that if something is Publishable *and* has User Values, the
    // Publishers currently won't see their User Values if there are changes to the Instance.
    // TODO (QI.7w4g8nd): this will need rationalization, especially once we get to Experiments. But by and
    // large, I expect to be bringing more stuff into here.
    pubState.map { ps =>
      ecology.api[Publication].enhanceState(stateWithUV, ps)
    }.getOrElse(stateWithUV)
  }

  /**
    * This is the dumping ground for exceptions to the rule that your Space only contains Things you can
    * read. There should *not* be many of these.
    */
  def isReadException(thingId:OID, user: User)(implicit state:SpaceState, ecology: Ecology):Boolean = {
    // I always have access to my own Person record, so that _me always works:
    ecology.api[Person].hasPerson(user, thingId)
  }
}
