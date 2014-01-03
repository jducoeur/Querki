package querki.ecology

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
   * Initializes the world.
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
 */
trait EcologyMember {
  /**
   * A way to get from here to the Ecology.
   */
  def ecology:Ecology
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
 */
trait Ecot extends EcologyMember {
  
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
}