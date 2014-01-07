package querki.ecology

import scala.reflect.runtime.universe.TypeTag

import querki.values.SpaceState

/**
 * The bundle that represents the entire universe.
 * 
 * The Ecology is all of the stateless functional parts of the system, wrapped up in
 * a single container. It is basically how Querki does dependency injection internally.
 * Essentially all *functional* components should live inside the Ecology as Ecots.
 * (Stateful components must instead live inside the Akka side of the world, so that
 * their threading is properly managed.)
 */
trait Ecology {
  /**
   * Gets the manager for this Ecology. Most of the time, you should be able to ignore
   * this -- it is mainly used for setup and shutdown.
   */
  def manager:EcologyManager
  
  /**
   * Fetches the registered interface. Throws an exception if the interface is not
   * registered, or not initialized.
   */
  def api[T <: EcologyInterface : TypeTag]:T
}

/**
 * Handles construction of the Ecology.
 * 
 * Yes, the EcologyManager and the Ecology are currently implemented in the same object.
 * Don't rely on that, though. The traits are separated in order to separate concerns,
 * and it is quite possible that the implementations might be separated at some point.
 */
trait EcologyManager {
  /**
   * Gets the Ecology that this is managing.
   */
  def ecology:Ecology
  
  /**
   * Adds the specified Ecot to this Ecology. Should be called during Ecot construction;
   * *must* be called before system initialization.
   * 
   * Order of registration is *entirely irrelevant*, and you should not count on it in any
   * way. Dependencies and initialization order are the important part.
   * 
   * IMPORTANT: for the time being, registration is assumed to be strictly synchronous. Do
   * *not* violate this rule! However, Ecots are allowed to create child Ecots within their
   * own constructors.
   */
  def register(ecot:Ecot):Unit
  
  /**
   * Initializes the world, and returns the resulting SpaceState.
   */
  def init(initialSpaceState:SpaceState):SpaceState
  
  /**
   * Terminates the world.
   */
  def term():Unit
  
  /**
   * Mainly for testing and reporting.
   */
  def isRegistered[C](implicit tag:TypeTag[C]):Boolean
}

/**
 * This is an abstraction for any object that lives inside the Ecology, and knows how to get to it.
 * This includes Ecots, but also many child objects of Ecots.
 * 
 * If at all possible, anything that "consumes" the Ecology -- anything that fetches interfaces -- should
 * be an EcologyMember. This means that it needs to receive the Ecology in some fashion -- most often as
 * a constructor parameter, but it could also simply use a pointer to its parent, or something like that.
 */
trait EcologyMember {
  /**
   * A way to get from here to the Ecology.
   */
  def ecology:Ecology
  
  /**
   * This is the method that Ecots and EcologyMembers should use to access other parts of the Ecology, if they are
   * *not* needed at initialization time. 
   */
  def interface[T <: EcologyInterface : TypeTag]:T = ecology.api[T]
}

/**
 * This is a pure marker trait. All "interfaces" exposed through the Ecology *must* have this as their
 * first trait linearly. (Usually, it will be the only thing that an exposed interface extends, but
 * that is not required.)
 */
trait EcologyInterface

case class InterfaceWrapper[T <: EcologyInterface](ecology:Ecology)(implicit tag:TypeTag[T]) {
  lazy val get:T = ecology.api[T]
}

/**
 * Definition of the Ids for an Ecot.
 * 
 * The ecotId parameter at the top is a global, and must be unique for each Ecot. The master
 * list of these is defined in querki.system.
 * 
 * This object should be defined at the package level, as part of the Ecot's API, so that
 * external systems can use these IDs safely, without causing accidental initialization of
 * the Ecot.
 */
class EcotIds(val ecotId:Short) {
  import models.OID

  /**
   * Defines an OID at the root system level. This is mainly exposed for older legacy Things,
   * that were defined before we began to break the world into Ecots. New Things should
   * *NEVER* use this -- use moid() instead!
   */
  def sysId(local:Int) = OID(0, local)
  
  /**
   * The OID for an Ecot-local Thing.
   * 
   * Each Ecot is required to declare the OIDs for the Things it defines, using this method.
   * 
   * moids should be permanent, just like the OIDs in SystemSpace. These are hardcoded values
   * that will be used in the database, so they *MUST* not change. If you need major changes,
   * deprecate the old value and introduce a new one.
   * 
   * You have 16 bits of namespace per Ecot. The theory is that that should be plenty for
   * any foreseeable Ecot. (Hopefully I won't regret this decision, but Modules aren't
   * supposed to be large.)
   * 
   * (Why "moid"? Ecots were originally called Modules, hence the "m", and it wasn't worth changing.)  
   */
  def moid(localId:Short):OID = {
    sysId((ecotId << 16) + localId)
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
    sysId(ecotId << 16 + localId)
  }
}

/**
 * A single "module" of the system.
 * 
 * In the Ecology Pattern, the world is mainly composed of Ecots. An Ecot is a piece of
 * arbitrary functionality. It may inject Things into the System Space, and in the long
 * run may inject its own Apps. It may implement any number of interfaces that are exposed
 * to the rest of the system.
 * 
 * Yes, Ecot is a horrible piece of jargon. But it is helpful to have a unique term, so
 * that nobody gets confused by highly-overloaded words like "module".
 * 
 * TODO: a lot of implementation details have bled into this interface. Fix the relationship
 * between it and Module. (Which will become EcotImpl.)
 */
trait Ecot extends EcologyMember {
  import scala.reflect.runtime.{universe => ru}
  import scala.reflect.runtime.universe._
  
  /**
   * The EcologyInterfaces that this Ecot requires in order to initialize.
   * 
   * Note that you will not usually set this manually.
   */
  def dependsUpon:Set[Class[_]] = _dependencies

  /**
   * This should be used by Ecots to pull in interfaces that they will need for initialization.
   * (This specifically includes Properties that are used in Thing declarations!)
   * 
   * The correct protocol here is to call this and stick the results in a *NON*-lazy val. The
   * returned value isn't the interface itself, but a wrapper that will dereference to the interface
   * during and after init. So it looks like this:
   * 
   *     ...
   *     val Foo = initRequires[querki.foo.Foo]
   *     ...
   *     lazy val MyProp = new SystemProperty(... stuff...,
   *       Foo("This is a Foo value"))
   *       
   * The key is that initRequires should be called synchronously during the constructor (and thereby
   * tells the Ecology that you have an init-time dependency), but the returned value can only be used
   * in lazy vals, or code that is called during init and later.
   */
  def initRequires[T <: EcologyInterface](implicit tag:TypeTag[T]):InterfaceWrapper[T] = {
    _dependencies += getClass(tag.tpe)
    InterfaceWrapper[T](ecology)
  }
  // Everything except System depends on System. This is kludgy but convenient:
  private var _dependencies:Set[Class[_]] = {
    this match {
      case me:querki.system.System => Set.empty
      case _ => Set(classOf[querki.system.System])
    }
  }
  
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
   * Note that registration takes place during construction, not necessarily at the end
   * of construction. You are explicitly *not* allowed to access the Ecology during construction,
   * or make any assumptions about being registered -- that is what init() is for.
   */
  ecology.manager.register(this)

  private def mirror(clazz:Class[_]):Mirror = ru.runtimeMirror(clazz.getClassLoader)
  private def getType[T](clazz: Class[T]):Type = {
    mirror(clazz).classSymbol(clazz).toType
  }
  private def getClass(tpe:Type):Class[_] = {
    mirror(this.getClass()).runtimeClass(tpe.typeSymbol.asClass)
  }

  /**
   * Mainly for printing.
   */
  def fullName = getType(this.getClass())
  
  /**
   * This is the set of all EcologyInterfaces that this Ecot implements.
   */
  def implements:Set[Class[_]] = {
    val markerTpe = getType(classOf[EcologyInterface])
    val markerName = markerTpe.toString()
    val clazz = this.getClass()
    val tpe = getType(clazz)
    // A base class is an EcologyInterface iff its *second* base class is EcologyInterface.
    // (Which means that that is the first thing it is extending.)
    val interfaceSymbols = tpe.baseClasses.filter { possibleInterfaceSymbol =>
      val possibleInterface = possibleInterfaceSymbol.typeSignatureIn(tpe)
      val interfaceBases = possibleInterface.baseClasses
      // Comparing by name is a bit lame, but I haven't yet figured out how to reliably
      // compare by identity:
      (interfaceBases.length > 1 &&
       interfaceBases(1).fullName == markerName)
    }
    interfaceSymbols.map(interface => mirror(clazz).runtimeClass(interface.asClass)).toSet
  }
  
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
  def addSystemObjects(state:SpaceState):SpaceState
}