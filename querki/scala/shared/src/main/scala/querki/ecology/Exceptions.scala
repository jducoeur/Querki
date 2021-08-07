package querki.ecology

class UninitializedInterfaceException(interface: Class[_])
  extends Exception(s"Attempting to fetch interface ${interface.getName()} before it has been initialized!")

class UnknownInterfaceException(interface: Class[_])
  extends Exception(s"Attempting to fetch unregistered interface ${interface.getName()}!")

class AlreadyRegisteredInterfaceException[ST, ET <: EcotBase[ST, ET]](
  interfaceClass: Class[_],
  currentRegistrant: ET,
  ecot: ET
) extends Exception(
    s"Ecot ${ecot.fullName} trying to register EcologyInterface ${interfaceClass.getSimpleName}, but it is already registered to ${currentRegistrant.fullName}"
  )

class InitMissingInterfaceException[ST, ET <: EcotBase[ST, ET]](
  missing: Class[_],
  usedBy: ET
) extends Exception(
    s"Initialization failed! Expected interface ${missing.getSimpleName()}, used by ${usedBy.fullName}, appears to be unimplemented."
  )

class InitDependencyLoopException[ST, ET <: EcotBase[ST, ET]](
  remaining: Set[ET],
  registered: Set[Class[_]]
) extends Exception(s"Initialization failed! There appears to be a dependency loop! Remaining Ecots:\n    " +
    remaining.map { ecot =>
      s"""    ${ecot.fullName}
         |      ${ecot.dependsUpon.filterNot(registered.contains(_)).map(_.getSimpleName).mkString(
        ", "
      )}""".stripMargin
    }.mkString("\n"))
