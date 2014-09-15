package querki.ecology.test

import scala.reflect.ClassTag

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
  def api[T <: EcologyInterface : ClassTag](implicit tag:ClassTag[T]):T
}

trait ClientState

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
   * 
   * TODO: this is different between Client and Server! Refactor this out, somehow. 
   */
  def init():Unit
  
  /**
   * Terminates the world.
   */
  def term():Unit
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
  implicit def ecology:Ecology
  
  /**
   * This is the method that Ecots and EcologyMembers should use to access other parts of the Ecology, if they are
   * *not* needed at initialization time. 
   */
  def interface[T <: EcologyInterface : ClassTag]:T = ecology.api[T]
}

/**
 * This is a pure marker trait. All "interfaces" exposed through the Ecology *must* have this as their
 * first trait linearly. (Usually, it will be the only thing that an exposed interface extends, but
 * that is not required.)
 */
trait EcologyInterface

case class InterfaceWrapper[T <: EcologyInterface](ecology:Ecology)(implicit tag:ClassTag[T]) {
  lazy val get:T = ecology.api[T]
}

// TODO: Server requires MOIDs; Client doesn't, right?

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
  def initRequires[T <: EcologyInterface](implicit tag:ClassTag[T]):InterfaceWrapper[T] = {
    _dependencies += tag.runtimeClass
    InterfaceWrapper[T](ecology)
  }
  private var _dependencies:Set[Class[_]] = Set.empty
  
  /**
   * Initialization call, which may be overridden by the Module. This should hook in
   * all Listeners.
   */
  def init = {}
  
  /**
   * Called after everything has finished initializing, but before we open the gates. This
   * is useful because you can call interfaces from here without introducing initRequires
   * dependencies. It is often useful for things like registering callbacks. By this point,
   * the main system Actors have been created, so you can *send* them messages, but preferably
   * should not block on the responses, since their Troupes may still be setting up.
   * 
   * postInit is not called in any particular order, and you should make no assumptions
   * about it.
   */
  def postInit = {}
  
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

  /**
   * Mainly for printing.
   */
  def fullName = this.getClass().getName()
  
  // TODO: this can't work the same way in Client. We might have to be explicit, sadly, although we might
  // be able to be horribly clever with Class and getSuperclass or getInterfaces instead.
  def implements:Set[Class[_]] = Set.empty
//  /**
//   * This is the set of all EcologyInterfaces that this Ecot implements.
//   */
//  def implements:Set[Class[_]] = {
//    val markerTpe = getType(classOf[EcologyInterface])
//    val markerName = markerTpe.toString()
//    val clazz = this.getClass()
//    val tpe = getType(clazz)
//    // A base class is an EcologyInterface iff its *second* base class is EcologyInterface.
//    // (Which means that that is the first thing it is extending.)
//    val interfaceSymbols = tpe.baseClasses.filter { possibleInterfaceSymbol =>
//      val possibleInterface = possibleInterfaceSymbol.typeSignatureIn(tpe)
//      val interfaceBases = possibleInterface.baseClasses
//      // Comparing by name is a bit lame, but I haven't yet figured out how to reliably
//      // compare by identity:
//      (interfaceBases.length > 1 &&
//       interfaceBases(1).fullName == markerName)
//    }
//    interfaceSymbols.map(interface => mirror(clazz).runtimeClass(interface.asClass)).toSet
//  }

  // TODO: there are a couple of methods here that are in the Server side, but not the Client.
  // Refactor these!
}