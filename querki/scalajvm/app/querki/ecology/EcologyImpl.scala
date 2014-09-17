package querki.ecology

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

import querki.values.SpaceState

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
    println("Starting Ecology initialization...")
    val mainState = initializeRemainingEcots(_registeredEcots, initialSpaceState)
    val finalState = specializedInit(mainState)
    postInitialize(_registeredEcots)
    finalState
  }
  
  def term():Unit = {
    _termOrder.foreach { ecot =>
      println(s"Terminating ecot ${ecot.fullName}")
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
    println(s"Initializing ecot ${ecot.fullName}")
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
      println("Ecology initialization complete")
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
          
          throw new InitDependencyLoopException[ST, ET](remaining)
        }
      }
    }
  }
  
  private def postInitialize(ecots:Set[ET]) = {
    ecots.foreach(_.postInit)
  }
}

class EcologyImpl extends EcologyImplBase[SpaceState, EcotImpl] {

  def init(initialSpaceState:SpaceState, createActorCb:CreateActorFunc):SpaceState = {
    val finalState = init(initialSpaceState) { state =>
      initializeActors(createActorCb)
      state
    }
    finalState
  }
  
  private def initializeActors(createActorCb:CreateActorFunc) = {
    _initializedEcots.foreach(_.createActors(createActorCb))
  }  
}