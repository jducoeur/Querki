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
package object globals {
  
  val Config = querki.util.Config
  val QLog = querki.util.QLog
  type PublicException = querki.util.PublicException
  
  type AnyProp = models.AnyProp
  
  type SpaceState = querki.values.SpaceState
  
  type OID = models.OID
  
  type Ecology = querki.ecology.Ecology
  type EcologyInterface = querki.ecology.EcologyInterface
  type EcologyMember = querki.ecology.EcologyMember
  type Ecot = querki.ecology.Ecot
  type QuerkiEcot = querki.ecology.QuerkiEcot
  implicit def wrapper2Interface[T <: EcologyInterface](wrapper:querki.ecology.InterfaceWrapperBase[SpaceState, querki.ecology.EcotImpl, T]):T = {
    wrapper.get
  }  
  
  /**
   * A quick-and-dirty temp wrapper to inject heavy spewage around some code while debugging.
   */
  def spewing[T](msg:String)(f: => T):T = {
    QLog.spew(s"Trying $msg")
    try {
      val result = f
      QLog.spew(s"  $msg succeeded, returning $result")
      result
    } catch {
      case ex:Exception => { QLog.error(s"  $msg failed", ex); throw ex }
    }
  }
  
  def spew(msg:String) = QLog.spew(msg)
  
  object Implicits {
    implicit lazy val execContext = scala.concurrent.ExecutionContext.Implicits.global
  }
}
