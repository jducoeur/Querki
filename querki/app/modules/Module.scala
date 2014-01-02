package modules

import models._
import models.system.OIDs

import querki.values.SpaceState

object Modules {
  
  // IMPORTANT: The numbers attached to these Modules must NEVER BE CHANGED!!!!! They
  // get built into the moid's, and thence into the database! If a Module is removed,
  // comment it out, but leave its number and all others alone.
  val Stylesheet = new stylesheet.StylesheetModule(1)
  val Email = new email.EmailModule(2)
  val Person = new person.PersonModule(3)
  val AccessControl = new querki.access.AccessControlModule(4)
  val Time = new time.TimeModule(5)
  val Collections = new collections.CollectionsModule(6)
//  val Rendering = new render.RenderingModule(7)
  val TOS = new querki.system.TOSModule(8)
  val Logic = new querki.logic.LogicModule(9)
  val Types = new querki.types.TypesModule(10)
  val UI = new querki.html.UIModule(11)
  val DeriveName = new querki.types.DeriveNameModule(12)
  val Editor = new querki.editing.EditorModule(13)
  val SkillLevel = new querki.identity.SkillLevelModule(14)
  val Conventions = new querki.conventions.ConventionsModule(15)
  val Core = new querki.core.CoreModule(16)
  val Basic = new querki.basic.BasicModule(17)
  
  private var allModules = Seq.empty[Module]
  
  def init(module:Module, state:SpaceState):SpaceState = {
    val newState = module.addSystemObjects(state)
    module.init
    // TODO: is this right? This looks suspiciously useless:
    module +: allModules
    newState
  }

  def initAllModules(state:SpaceState):SpaceState = {
    var s = state
    
    // TODO: we shouldn't do this explicitly, we should declare these things just once:
    // TODO: in the long run, these should self-declare their dependencies, and
    // do a topological sort to initialize and terminate them in order:
    s = init(Core, s)
    s = init(Types, s)
    s = init(Conventions, s)
    s = init(Basic, s)
    s = init(Stylesheet, s)
    s = init(Email, s)
    s = init(Person, s)
    s = init(AccessControl, s)
    s = init(Time, s)
    s = init(Collections, s)
//    s = init(Rendering, s)
    s = init(TOS, s)
    s = init(Logic, s)
    s = init(UI, s)
    s = init(DeriveName, s)
    s = init(Editor, s)
    s = init(SkillLevel, s)
    
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
 * Definition of the ModuleIds for a Module.
 * 
 * The moduleId parameter at the top is a global, and must be unique for each Module. The master
 * list of these is defined in Modules itself.
 * 
 * This object should be defined at the package level, as part of the Module's API, so that
 * external systems can use these IDs safely, without causing accidental initialization of
 * the Module.
 */
class ModuleIds(val moduleId:Short) {
  
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
    OIDs.sysId((moduleId << 16) + localId)
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
 * 
 * 
 * =================
 * Building a Module
 * =================
 * 
 * A Module's constructor should be completely self-contained -- it should not have any
 * external references to runtime data. That has an *extremely* important corollary: it
 * shouldn't construct anything that refers to external objects. That in turn means that
 * it shouldn't use OIDs from other Modules. And that, in turn, tends to imply that you
 * shouldn't define any Things in the constructor.
 * 
 * Instead, Things should usually be created during init(). This will build everything
 * *after* all of the Modules have been created, so all the MOIDs will exist. It also
 * creates everything in DependsUpon order, so that Modules can depend on each other.
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
    OIDs.sysId((moduleId << 16) + localId)
  }
  
  /**
   * The old, broken algorithm for calculating moids. This was a *horrible* bug, and wound
   * up polluting the OID space for a couple dozen Things. The only saving grace is that this
   * error winds up with the lower 16 bits empty, so the results can't collide with correctly-formed
   * moids.
   * 
   * TODO: go through the existing Spaces, and rewrite all references to these old moids to new
   * ones that are correct. This is going to be delicate work.
   */
  def oldMoid(localId:Short):OID = {
    OIDs.sysId(moduleId << 16 + localId)
  }
  
  /**
   * The PTypes introduced by this Module, if any.
   */
  lazy val types:Seq[PType[_]] = Seq.empty
  /**
   * The Properties introduced by this Module, if any.
   */
  lazy val props:Seq[Property[_,_]] = Seq.empty
  /**
   * The Things (usually Models, but not necessarily) introduced by this Module, if any.
   */
  lazy val things:Seq[ThingState] = Seq.empty
  
  /**
   * If the Module requires any specialized Things, Properties, Types or Collections,
   * add them to the state here. The returned state should be the passed-in one plus
   * the Module's stuff. (Note that the passed-in state is the System Space -- you are
   * adding stuff to System.)
   * 
   * Individual Modules should not usually need to override this; instead, just define
   * your objects in the types, props and things properties, and they will be added
   * automatically.
   */
  def addSystemObjects(state:SpaceState):SpaceState = {
//    play.api.Logger.info("----> addSystemObjects for module " + moduleId + "; props are " + props.map{prop => prop.id.toString + "/" + prop.displayName}.toList.sorted.mkString(", "))
//    play.api.Logger.info("---->     keys start as " + state.spaceProps.keys.map(_.toString).toList.sorted.mkString(", "))
//    
    val result = state.copy(
      spaceProps = OIDMap[Property[_,_]](props:_*) ++: state.spaceProps, 
      things = OIDMap[ThingState](things:_*) ++: state.things,
      types = OIDMap[PType[_]](types:_*) ++: state.types)
//      
//    play.api.Logger.info("---->     all props are now " + result.spaceProps.values.map(_.displayName).toList.sorted.mkString(", "))
    
    result
  }
  
}