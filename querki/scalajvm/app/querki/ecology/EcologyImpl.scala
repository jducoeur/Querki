package querki.ecology

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

import akka.actor.{ActorRef, Props}

import querki.values.SpaceState

class EcologyImpl extends Ecology with EcologyManager {
  
  // ******************************************************
  //
  // EcologyManager Implementation
  //
  
  val ecology:Ecology = this
  val runtimeMirror = ru.runtimeMirror(this.getClass().getClassLoader)
  
  def register(ecot:Ecot):Unit = {
    _registeredEcots = _registeredEcots + ecot
    
    ecot.implements.foreach { interfaceClass => 
      if (_registeredInterfaces.contains(interfaceClass)) {
        val currentRegistrant = _registeredInterfaces(interfaceClass)
        throw new AlreadyRegisteredInterfaceException(interfaceClass, currentRegistrant, ecot)
      } else {
        _registeredInterfaces = _registeredInterfaces + (interfaceClass -> ecot)
      }
    }
  }
  
  private def getType[T](clazz: Class[T]):Type = {
    val runtimeMirror = ru.runtimeMirror(clazz.getClassLoader)
    runtimeMirror.classSymbol(clazz).toType
  }
  
  def init(initialSpaceState:SpaceState, createActorCb:CreateActorFunc):SpaceState = {
    println("Starting Ecology initialization...")
    val finalState = initializeRemainingEcots(_registeredEcots, initialSpaceState)
    initializeActors(createActorCb)
    postInitialize(_registeredEcots)
    finalState
  }
  
  def term():Unit = {
    _termOrder.foreach { ecot =>
      println(s"Terminating ecot ${ecot.fullName}")
      ecot.term
    }
  }

  def isRegistered[C](implicit tag:TypeTag[C]):Boolean = {
    val clazz = runtimeMirror.runtimeClass(tag.tpe.typeSymbol.asClass)
    _registeredInterfaces.contains(clazz)
  }
  
  // ******************************************************
  //
  // Ecology Implementation
  //
  
  val manager:EcologyManager = this
  
  def api[T <: EcologyInterface : TypeTag]:T = {
    // This is a bit dubiously inefficient. But it is supposed to mainly be called via
    // InterfaceWrapper.get, which caches the result, so it shouldn't be called *too* often
    // after system initialization.
    val clazz = runtimeMirror.runtimeClass(typeOf[T].typeSymbol.asClass)
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
  private var _registeredEcots:Set[Ecot] = Set.empty
  
  /**
   * All of the EcologyInterfaces that have been registered, and which Ecot implements each.
   * 
   * TODO: could this be done with Map[Type] instead of Map[Class]? Given Scala's preferences, that
   * might be more efficient. And I don't think there is anything terribly public that has wound
   * up relying on Class -- we're actually mostly using TypeTag in the APIs.
   */
  private var _registeredInterfaces:Map[Class[_], Ecot] = Map.empty
  
  /**
   * All of the Ecots that have been fully initialized.
   */
  private var _initializedEcots:Set[Ecot] = Set.empty
  
  /**
   * All of the EcologyInterfaces that have been fully initialized. Once they have been initialized, other systems may
   * access them.
   */
  private var _initializedInterfaces:Map[Class[_], Ecot] = Map.empty
  
  /**
   * The calculated order to terminate the world. This is basically the reverse of the order in which we
   * initialized, since we calculated the dependencies then.
   */
  private var _termOrder:List[Ecot] = Nil
  
  def initEcot(ecot:Ecot, currentState:SpaceState):SpaceState = {
    // TODO: this should go through Log instead:
    println(s"Initializing ecot ${ecot.fullName}")
    val newState = ecot.addSystemObjects(currentState)
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
  private def initializeRemainingEcots(remaining:Set[Ecot], currentState:SpaceState):SpaceState = {
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
                throw new InitMissingInterfaceException(dependency, ecot)
            }
          }
          
          throw new InitDependencyLoopException(remaining)
        }
      }
    }
  }
  
  private def initializeActors(createActorCb:CreateActorFunc) = {
    _initializedEcots.foreach(_.createActors(createActorCb))
  }
  
  private def postInitialize(ecots:Set[Ecot]) = {
    ecots.foreach(_.postInit)
  }
}