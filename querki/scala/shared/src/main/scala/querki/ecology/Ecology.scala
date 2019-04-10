package querki.ecology

import scala.reflect.ClassTag

/**
 * This is a pure marker trait. All "interfaces" exposed through the Ecology *must* have this as their
 * first trait linearly. (Usually, it will be the only thing that an exposed interface extends, but
 * that is not required.)
 */
trait EcologyInterface

/**
 * The bundle that represents the entire universe.
 * 
 * The Ecology is all of the stateless functional parts of the system, wrapped up in
 * a single container. It is basically how Querki does dependency injection internally.
 * Essentially all *functional* components should live inside the Ecology as Ecots.
 * (Stateful components must instead live inside the Akka side of the world, so that
 * their threading is properly managed.)
 */
trait EcologyBase[ST, ET <: EcotBase[ST, ET]] {
  /**
   * Gets the manager for this Ecology. Most of the time, you should be able to ignore
   * this -- it is mainly used for setup and shutdown.
   */
  def manager:EcologyManager[ST, ET]
  
  /**
   * Fetches the registered interface. Throws an exception if the interface is not
   * registered, or not initialized.
   */
  def api[T <: EcologyInterface](implicit tag:ClassTag[T]):T
}

/**
 * Handles construction of the Ecology.
 * 
 * Yes, the EcologyManager and the Ecology are currently implemented in the same object.
 * Don't rely on that, though. The traits are separated in order to separate concerns,
 * and it is quite possible that the implementations might be separated at some point.
 */
trait EcologyManager[ST, ET <: EcotBase[ST, ET]] {
  /**
   * Gets the Ecology that this is managing.
   */
  def ecology:EcologyBase[ST, ET]
  
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
  def register(ecot:ET):Unit
  
  /**
   * Terminates the world.
   */
  def term():Unit
  
  /**
   * Mainly for testing and reporting.
   */
  def isRegistered[C](implicit tag:ClassTag[C]):Boolean
}

/**
 * This is an abstraction for any object that lives inside the Ecology, and knows how to get to it.
 * This includes Ecots, but also many child objects of Ecots.
 * 
 * If at all possible, anything that "consumes" the Ecology -- anything that fetches interfaces -- should
 * be an EcologyMember. This means that it needs to receive the Ecology in some fashion -- most often as
 * a constructor parameter, but it could also simply use a pointer to its parent, or something like that.
 */
trait EcologyMemberBase[ST, ET <: EcotBase[ST, ET]] {
  /**
   * A way to get from here to the Ecology.
   */
  implicit def ecology:EcologyBase[ST, ET]
  
  /**
   * This is the method that Ecots and EcologyMembers should use to access other parts of the Ecology, if they are
   * *not* needed at initialization time. 
   */
  def interface[T <: EcologyInterface : ClassTag]:T = ecology.api[T]
}

case class InterfaceWrapperBase[ST, ET <: EcotBase[ST, ET], T <: EcologyInterface](ecology:EcologyBase[ST, ET])(implicit tag:ClassTag[T]) {
  lazy val get:T = ecology.api[T]
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
 * 
 * @tparam ST The StateType that this Ecot type works with. Used in initialization, if we
 *     are building up some sort of initial system state.
 */
trait EcotBase[ST, ET <: EcotBase[ST, ET]] extends EcologyMemberBase[ST, ET] { self:ET =>

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
   * If this Ecot needs to add something to the initial system state, do it here.
   */
  def addState(prevState:ST):ST = { prevState }
  
  /**
   * Note that registration takes place during construction, not necessarily at the end
   * of construction. You are explicitly *not* allowed to access the Ecology during construction,
   * or make any assumptions about being registered -- that is what init() is for.
   */
  ecology.manager.register(this)
  
  /**
   * This is intentionally left blank in EcotBase, because the implementation differs between
   * the ScalaJVM side (which can do horribly clever things with Types) and the ScalaJS side
   * (which can't).
   * 
   * TBD: can we rewrite implements() as a macro which would work on both sides? Maybe.
   */
  def implements:Set[Class[_]]
  
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
  def initRequires[T <: EcologyInterface](implicit tag:ClassTag[T]):InterfaceWrapperBase[ST, ET, T] = {
    _dependencies += tag.runtimeClass
    InterfaceWrapperBase[ST, ET, T](ecology)
  }
  private var _dependencies:Set[Class[_]] = Set.empty

  /**
   * Mainly for printing.
   */
  def fullName = this.getClass().getName()
}

class EcologyImplBase[ST, ET <: EcotBase[ST, ET]] extends EcologyBase[ST, ET] with EcologyManager[ST, ET] {
  
  // ******************************************************
  //
  // EcologyManager Implementation
  //
  
  val ecology:EcologyBase[ST, ET] = this
  
  def register(ecot:ET):Unit = {
    _registeredEcots = _registeredEcots + ecot
    
    ecot.implements.foreach { interfaceClass => 
      if (_registeredInterfaces.contains(interfaceClass)) {
        val currentRegistrant = _registeredInterfaces(interfaceClass)
        throw new AlreadyRegisteredInterfaceException[ST, ET](interfaceClass, currentRegistrant, ecot)
      } else {
        _registeredInterfaces = _registeredInterfaces + (interfaceClass -> ecot)
      }
    }
  }
  
  def init(initialSpaceState:ST)(specializedInit:ST => ST):ST = {
//    println("Starting Ecology initialization...")
    val mainState = initializeRemainingEcots(_registeredEcots, initialSpaceState)
//	println("Main initialization complete")
    val finalState = specializedInit(mainState)
//	println("SpecializedInit complete")
    postInitialize(_registeredEcots)
//	println("PostInit complete")
    finalState
  }
  
  def term():Unit = {
    _termOrder.foreach { ecot =>
//      println(s"Terminating ecot ${ecot.fullName}")
      ecot.term
    }
  }

  def isRegistered[C](implicit tag:ClassTag[C]):Boolean = {
    val clazz = tag.runtimeClass
    _registeredInterfaces.contains(clazz)
  }
  
  // ******************************************************
  //
  // Ecology Implementation
  //
  
  val manager:EcologyManager[ST, ET] = this
  
  def api[T <: EcologyInterface](implicit tag:ClassTag[T]):T = {
    // This is a bit dubiously inefficient. But it is supposed to mainly be called via
    // InterfaceWrapper.get, which caches the result, so it shouldn't be called *too* often
    // after system initialization.
    val clazz = tag.runtimeClass
    try {
      _initializedInterfaces.get(clazz) match {
        case Some(ecot) => ecot.asInstanceOf[T]
        case None => {
          if (_registeredInterfaces.contains(clazz))
            throw new UninitializedInterfaceException(clazz)
          else
            throw new UnknownInterfaceException(clazz)
        }
      }
    } catch {
      case e:Exception => {
        // TODO: this should use logger, not println, once that mechanism is nice and solid:
        println(s"Exception $e while trying to fetch interface ${clazz.getSimpleName()}:")
        e.printStackTrace()
        throw e
      }
    }    
  }
  
  // ******************************************************
  //
  // Internals
  //
  
  /**
   * All of the Ecots that have been registered, in no particular order.
   */
  private var _registeredEcots:Set[ET] = Set.empty
  
  /**
   * All of the EcologyInterfaces that have been registered, and which Ecot implements each.
   * 
   * TODO: could this be done with Map[Type] instead of Map[Class]? Given Scala's preferences, that
   * might be more efficient. And I don't think there is anything terribly public that has wound
   * up relying on Class -- we're actually mostly using TypeTag in the APIs.
    *
    * TODO: in principle, using Class as a key in a Map isn't ideal, since it prevents GC in a more
    * dynamic environment. This isn't critical for Querki, but might be an issue for some applications if
    * this becomes a library. Java now has a ClassValue type that is more appropriate for this purpose,
    * and Sebastien seems at least open to adding it to Scala.js. See this thread on Contributors:
    *
    * https://contributors.scala-lang.org/t/proposal-for-opaque-type-aliases/2947/51
    *
    * and the linked SO thread:
    *
    * https://stackoverflow.com/questions/7444420/classvalue-in-java-7
    *
    * Note that this applies to all the other Maps and Sets where we are currently storing Classes.
   */
  private var _registeredInterfaces:Map[Class[_], ET] = Map.empty
  
  /**
   * All of the Ecots that have been fully initialized.
   */
  protected var _initializedEcots:Set[ET] = Set.empty
  
  /**
   * All of the EcologyInterfaces that have been fully initialized. Once they have been initialized, other systems may
   * access them.
   */
  private var _initializedInterfaces:Map[Class[_], ET] = Map.empty
  
  /**
   * The calculated order to terminate the world. This is basically the reverse of the order in which we
   * initialized, since we calculated the dependencies then.
   */
  private var _termOrder:List[ET] = Nil
  
  def initEcot(ecot:ET, currentState:ST):ST = {
    // TODO: this should go through Log instead:
//    println(s"Initializing ecot ${ecot.fullName}")
    val newState = ecot.addState(currentState)
    ecot.init
    _initializedEcots += ecot
    _termOrder = ecot :: _termOrder
    ecot.implements.foreach(interface =>_initializedInterfaces += (interface -> ecot))
    newState
  }
  
  /**
   * Recursively initialize the system. In each recursive pass, go through the remaining Ecots, and initialize
   * the first one we find that has no uninitialized dependencies. If we get through a pass without being able to
   * initialize *anything*, we have failed.
   * 
   * One practical detail that is Querki-specific: as we go, we add each Ecot's Things to the SystemSpace.
   */
  private def initializeRemainingEcots(remaining:Set[ET], currentState:ST):ST = {
    if (remaining.isEmpty) {
//      println("Ecology initialization complete")
      currentState
    } else {
      remaining.find(_.dependsUpon.forall(_initializedInterfaces.contains(_))) match {
        case Some(readyEcot) => {
          val newState = initEcot(readyEcot, currentState)
          initializeRemainingEcots(remaining - readyEcot, newState)
        }
        // TODO: scan the remainder, and particularly their dependencies. If we find a dependency that
        // isn't in _registeredInterfaces, that means something isn't implemented yet. Otherwise, it
        // indicates a dependency loop. Include all of the remainder in an error message.
        case None => {
          remaining.foreach { ecot =>
            ecot.dependsUpon.foreach { dependency =>
              if (!_registeredInterfaces.contains(dependency))
                throw new InitMissingInterfaceException[ST, ET](dependency, ecot)
            }
          }
          
          throw new InitDependencyLoopException[ST, ET](remaining, _initializedInterfaces.keySet)
        }
      }
    }
  }
  
  private def postInitialize(ecots:Set[ET]) = {
    ecots.foreach { ecot =>
	  try {
	    ecot.postInit 
	  } catch {
	    case ex:Exception => {
		  println(s"Got exception while post-initializing ${ecot.fullName}: $ex")
		  throw ex
		}
	  }
	}
  }
}
