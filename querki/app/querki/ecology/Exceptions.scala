package querki.ecology

class UninitializedInterfaceException(interface:Class[_]) extends Exception(s"Attempting to fetch interface ${interface.getName()} before it has been initialized!")
class UnknownInterfaceException(interface:Class[_]) extends Exception(s"Attempting to fetch unregistered interface ${interface.getName()}!")
class AlreadyRegisteredInterfaceException(interfaceClass:Class[_], currentRegistrant:Ecot, ecot:Ecot)
  extends Exception(s"Ecot ${ecot.fullName} trying to register EcologyInterface ${interfaceClass.getSimpleName}, but it is already registered to ${currentRegistrant.fullName}")