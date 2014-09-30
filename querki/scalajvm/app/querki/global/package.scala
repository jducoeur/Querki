package querki

/**
 * Standard includes for Server-side Querki. The intent here is that most files should say:
 * {{{
 * import querki.global._
 * }}}
 * and thereby fetch the classes that are almost universally wanted. Ideally, this should be
 * the only full-package import from Querki itself.
 * 
 * IMPORTANT: do not put the kitchen sink in here! This should be carefully thought out, and
 * classes should only go in here if they are desireable in at least half of implementation
 * files. Otherwise, it is begging for compile slowdowns!
 */
package object global {
  
  val Config = querki.util.Config
  val QLog = querki.util.QLog
  
  type SpaceState = querki.values.SpaceState
  
  type Ecology = querki.ecology.Ecology
  type EcologyInterface = querki.ecology.EcologyInterface
  type EcologyMember = querki.ecology.EcologyMember
  type Ecot = querki.ecology.Ecot
  type QuerkiEcot = querki.ecology.QuerkiEcot
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:querki.ecology.InterfaceWrapperBase[SpaceState, querki.ecology.EcotImpl, T]):T = {
    wrapper.get
  }  
  
  object Implicits {
    implicit lazy val execContext = scala.concurrent.ExecutionContext.Implicits.global
  }
}
