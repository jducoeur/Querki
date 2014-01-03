package querki.ecology

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

class EcologyImpl extends Ecology with EcologyManager {
  
  // ******************************************************
  //
  // EcologyManager Implementation
  //
  
  val ecology:Ecology = this
  val runtimeMirror = ru.runtimeMirror(this.getClass().getClassLoader)
  
  def register(ecot:Ecot):Unit = {
    _registeredEcots = _registeredEcots + ecot
    
//    println(s"Registered Ecot ${getType(ecot.getClass())}")
    ecot.implements.foreach { interface => 
      val interfaceClass = runtimeMirror.runtimeClass(interface.asClass)
//      println(s"    ${interface.fullName}")
      if (_registeredInterfaces.contains(interfaceClass)) {
        val currentRegistrant = _registeredInterfaces(interfaceClass)
        throw new Exception(s"Ecot ${ecot.fullName} trying to register EcologyInterface ${interface.fullName}, but it is already registered to ${currentRegistrant.fullName}")
      } else {
        _registeredInterfaces = _registeredInterfaces + (interfaceClass -> ecot)
      }
    }
    
//    
//    // EXPERIMENTAL:
//    val markerTpe = getType(classOf[EcologyInterface])
//    val markerName = markerTpe.toString()
//    val clazz = ecot.getClass()
//    val tpe = getType(clazz)
//    println(s"Registered Ecot of type $tpe:")
//    tpe.baseClasses.foreach { base =>
//      val baseTpe = base.typeSignatureIn(tpe)
//      val baseBases = baseTpe.baseClasses
//      if (baseBases.length > 1) {
//        val possibleMarkerSymbol = baseBases(1)
//        val possibleMarkerName = possibleMarkerSymbol.fullName
//        if (possibleMarkerName == markerName) {
//          println(s"    ${base.fullName}")          
//        }
//      }
//    }
  }
  
  def getType[T](clazz: Class[T]):Type = {
    val runtimeMirror = ru.runtimeMirror(clazz.getClassLoader)
    runtimeMirror.classSymbol(clazz).toType
  }
  
  def init():Unit = ???
  
  def term():Unit = ???

  def allRegisteredInterfaces:Set[Class[_]] = _registeredInterfaces.keys.toSet
  
  // ******************************************************
  //
  // Ecology Implementation
  //
  
  val manager:EcologyManager = this  
  
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
   */
  private var _registeredInterfaces:Map[Class[_], Ecot] = Map.empty  
}