package querki

import scala.util.{Success, Failure}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

package object util {
  /**
   * This provides a variant of spewing(), specifically for Futures.
   */
  implicit class QLogFuture[T](fut:Future[T]) {
    def spewFut(msg:String):Future[T] = {
      QLog.spew(s"Prepping $msg")
      fut.onComplete {
        case Success(s) => QLog.spew(s"  ... $msg succeeded with $s")
        case Failure(ex) => QLog.error(s"  ... $msg failed", ex)
      }
      fut
    }
  }
}
