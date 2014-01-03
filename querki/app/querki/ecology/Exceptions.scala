package querki.ecology

class UninitializedInterfaceException(interface:Class[_]) extends Exception(s"Attempting to fetch interface ${interface.getName()} before it has been initialized!")
class UnknownInterfaceException(interface:Class[_]) extends Exception(s"Attempting to fetch unregistered interface ${interface.getName()}!")
