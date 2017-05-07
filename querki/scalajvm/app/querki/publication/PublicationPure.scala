package querki.publication

import PublicationEvents._

/**
 * The pure-functional heart of the Publication system.
 * 
 * For the time being, this is completely overkill -- it does almost nothing. It's here mostly on general
 * principles, in case the system gets more interesting.
 */
trait PublicationPure {
  def addPublication(evt:RawPublishEvent, state:PublicationState):PublicationState = {
    state.copy(events = state.events :+ evt)(state.ecology)
  }
}
