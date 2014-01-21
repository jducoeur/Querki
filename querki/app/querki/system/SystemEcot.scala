package querki.system

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
}

object SystemMOIDs extends EcotIds(18)

class SystemEcot(e:Ecology) extends QuerkiEcot(e) with System with SystemManagement {
  
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
            |[[Tag Set Type -> 
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
}
