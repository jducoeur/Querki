package querki.system

import akka.actor.ActorSystem

import querki.ecology._
import querki.values.SpaceState

/**
 * The interface to manage the System.
 */
trait SystemManagement extends EcologyInterface {
  /**
   * Set the final System Space. The code that initialized the Ecology *must* call this once complete!
   */
  def setState(state:SpaceState)
  
  /**
   * The one true ActorSystem that everybody is living in. Ecots should obtain the ActorSystem through this,
   * if they need it.
   * 
   * Note the explicit assumption that there is a 1-to-1 correspondence between the Ecology and the
   * ActorSystem.
   * 
   * This will throw an exception if called when there is no ActorSystem, as in unit tests!
   */
  def actorSystem:ActorSystem
}

object SystemMOIDs extends EcotIds(18)

class SystemEcot(e:Ecology, actorSystemOpt:Option[ActorSystem]) extends QuerkiEcot(e) with System with SystemManagement {
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Tags = interface[querki.tags.Tags]
  
  // This is called when the world finishes up:
  def setState(stateIn:SpaceState) = {
    // Now that all of the Properties exist, we can modify the System Space to use them:
    val state = stateIn.copy(pf = 
      toProps(
        setName("System"),
        Basic.DisplayTextProp("""### Things in [[Display Name]]
            |[[All Things]]
            |
            |[[Tag Type -> 
            |_propsOfType ->
            |_sort ->
            |_section(
            |""### Tags"", 
            |""**____**: [[_tagsForProperty -> _sort -> _join("", "")]]
            |"")]]
            |
            |[[Old Tag Set Type -> 
            |_propsOfType ->
            |_sort ->
            |_section(
            |""### Tags"", 
            |""**____**: [[_tagsForProperty -> _sort -> _join("", "")]]
            |"")]]
            |
            |[[How It Works -> _if(_isDefined, ""**____**"")]]
            |""".stripMargin),
        Tags.ShowUnknownProp(querki.tags.defaultDisplayText)))
    _state = Some(state)
  }
  
  var _state:Option[SpaceState] = None
  def State = _state.getOrElse(throw new Exception("Attempting to access the System Space before init is complete!"))
  
  def actorSystem = actorSystemOpt.get
}
