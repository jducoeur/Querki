package modules

import models._
import models.system.OIDs

object Modules {
  
  private var allModules = Seq.empty[Module]
  
  def init(module:Module, state:SpaceState):SpaceState = {
    val newState = module.addSystemObjects(state)
    module.init
    module +: allModules
    newState
  }

  def initAllModules(state:SpaceState):SpaceState = {
    var s = state
    
    s = init(new stylesheet.StylesheetModule(1), s)
    
    s
  }
  
  /**
   * Calls term on all Modules, for system shutdown. Each Module should terminate
   * cleanly and completely before returning. Note that this terminates in reverse
   * order of initialization, so that dependencies unwind properly. (Assuming we
   * wind up implementing dependencies.)
   */
  def termAllModules = {
    allModules.foreach(_.term)
  }
}

/**
 * Represents a "plug-in" part of the system.
 * 
 * A Module is a collection of Properties, Things, Listeners and (typically) some code to
 * integrate with specialized libraries, which adds a particular kind of capability to
 * Querki.
 * 
 * Note that Modules do *not* exist mainly for third-party use. We might well use them that
 * way a bit -- in particular, folks who pick up Querki open-source to customize for their
 * own use are likely to want to add specialized modules. But they are really mainly intended
 * for code cleanliness. In principle, anything built into the system that is not truly
 * core functionality belongs in a cohesive Module that serves a specific purpose.
 * 
 * Note the corollary: the core code should never depend on a Module. This will introduce
 * a fair number of callbacks and indirections, but should pay off in the long run. But try
 * not to be too opaque; the consequence of using the Listener Pattern a lot is that we will
 * sometime suffer from "How the heck did that happen?"
 * 
 * To enforce this requirement that core code shouldn't depend on Modules, they should
 * generally go in their own packages, one per Module. Those shouldn't be referenced anywhere
 * but here.
 * 
 * As of this writing, the Module system is still growing. I expect that it will eventually
 * hook a *lot* of different events in the system, so you can add lots of different features.
 * This is fine, but note that these listeners are always required to be *fast* -- any slow
 * actions (where "slow" is certainly anything that takes longer than a millisecond, and
 * preferably less than that) must happen in separate Actors.
 */
trait Module {
  
  /**
   * Mandatory value for concrete classes to fill in.
   */
  val moduleId:Short
  
  /**
   * Initialization call, which may be overridden by the Module. This should hook in
   * all Listeners.
   */
  def init = {}
  
  /**
   * Termination call, which may be overridden by the Module on system shutdown.
   */
  def term = {}
  
  /**
   * The OID for a Module-local Thing.
   * 
   * It is strongly recommended that each Module define a central table of its local OIDs,
   * similar to the way SystemSpace.OIDs does, to avoid namespace contention.
   * 
   * moids should be permanent, just like the OIDs in SystemSpace. These are hardcoded values
   * that will be used in the database, so they *MUST* not change. If you need major changes,
   * deprecate the old value and introduce a new one.
   * 
   * You have 16 bits of namespace per Module. The theory is that that should be plenty for
   * any foreseeable Module. (Hopefully I won't regret this decision, but Modules aren't
   * supposed to be large.)  
   */
  def moid(localId:Short):OID = {
    OIDs.sysId(moduleId << 16 + localId)
  }
  
  /**
   * If the Module requires any specialized Things, Properties, Types or Collections,
   * add them to the state here. The returned state should be the passed-in one plus
   * the Module's stuff. (Note that the passed-in state is the System Space -- you are
   * adding stuff to System.)
   */
  def addSystemObjects(state:SpaceState):SpaceState = { state }
}