package querki.ecology.test

class UninitializedInterfaceException(interface:Class[_]) extends Exception(s"Attempting to fetch interface ${interface.getName()} before it has been initialized!")

class UnknownInterfaceException(interface:Class[_]) extends Exception(s"Attempting to fetch unregistered interface ${interface.getName()}!")

class AlreadyRegisteredInterfaceException(interfaceClass:Class[_], currentRegistrant:Ecot, ecot:Ecot)
  extends Exception(s"Ecot ${ecot.fullName} trying to register EcologyInterface ${interfaceClass.getSimpleName}, but it is already registered to ${currentRegistrant.fullName}")

class InitMissingInterfaceException(missing:Class[_], usedBy:Ecot) 
  extends Exception(s"Initialization failed! Expected interface ${missing.getSimpleName()}, used by ${usedBy.fullName}, appears to be unimplemented.")

class InitDependencyLoopException(remaining:Set[Ecot])
  extends Exception(s"Initialization failed! There appears to be a dependency loop! Remaining Ecots:\n    " +
      remaining.map(_.fullName).mkString("\n    "))
