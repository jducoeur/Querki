package querki.util

import scala.util._

import play.api.i18n.Messages
import play.api.mvc.RequestHeader

import controllers.PlayRequestContext
import querki.values.RequestContext

/**
 * Represents an error that is intended to be displayed to end users. Intentionally forces you to
 * internationalize the message properly. All exceptions that are to be shown to users should use this!
 */
case class PublicException(msgName:String, params:Any*) extends Exception {
  def display(implicit req:RequestHeader):String = Messages(msgName, params:_*)
  def display(rc:Option[RequestContext]):String = {
    rc match {
      case Some(prc:PlayRequestContext) => display(prc.request)
      // This will default to the system default language.
      // TODO: we should have a concept of Lang available in RequestContext itself!
      case _ => Messages(msgName, params:_*)
    }
  }
  override def getMessage = "BUG: Trying to display a PublicException without the Request. Use display() instead."
}
object UnexpectedPublicException extends PublicException("General")

/**
 * Represents an internal error that should *not* be shown to end users.
 */
case class InternalException(message:String) extends Exception(message)

/**
 * This captures the common pattern we want to encourage, using Try and PublicException. Note
 * that while this can return a value, it is particularly useful in Actors, where the real action
 * is side-effectful (usually sending messages) and it returns Unit.
 * 
 * Basically, this takes three blocks. The first does the actual calculation. The second does something
 * with the result if it succeeds. The third does something with the resulting PublicException if it
 * fails.
 * 
 * TBD: this is almost *too* concise: it is arguably hard to distinguish the success and failure cases,
 * especially since you don't actually have to declare any of the types. We may actually want some
 * syntactic glue to clarify.
 */
object Tryer {
  def apply[T, R](func: => T)(succ:T => R)(fail:PublicException => R):R = {
    val t = Try { func }
    t match {
      case Failure(ex:PublicException) => fail(ex)
      case Failure(error) => { QLog.error("Internal error", error); fail(UnexpectedPublicException) }
      case Success(v) => succ(v)
    }
  }
}

// Trying to get a version of this that works with an ordinary Try. Still a project in-progress.
//object RichTry {
//  class TryHandler[T, R](t:Try[T], succ:Option[T => R] = None, fail:Option[PublicException => R] = None) {
//    def onSucc(f:T => R) = new TryHandler(t, Some(f), fail)
//    def onFail(f:PublicException => R) = new TryHandler(t, succ, Some(f))
//    
//    def result:R = {
//      t match {
//        case Failure(ex:PublicException) if (fail.isDefined) => fail.get(ex)
//        case Failure(error) if (fail.isDefined) => { QLog.error("Internal error", error); fail.get(UnexpectedPublicException) }
//        case Success(v) if (succ.isDefined) => succ.get(v)
//        case _ => throw new Exception("Incompletely defined TryTrans")
//      }    
//    }
//  }
//  
//  implicit class TryWrapper[T, R](t:Try[T]) {
//    def onSucc(f:T => R) = new TryHandler[T, R](t, Some(f), None)
//    def onFail(f:PublicException => R) = new TryHandler[T, R](t, None, Some(f))    
//  }
//}

class TryTrans[T, R](func: => T, succ:Option[T => R] = None, fail:Option[PublicException => R] = None) {
  type TRet = R
  def onSucc(f:T => R) = new TryTrans(func, Some(f), fail)
  def onFail(f:PublicException => R) = new TryTrans(func, succ, Some(f))
  
  def result:R = {
    val t = Try { func }
    t match {
      case Failure(ex:PublicException) if (fail.isDefined) => fail.get(ex)
      case Failure(error) if (fail.isDefined) => { QLog.error("Internal error", error); fail.get(UnexpectedPublicException) }
      case Success(v) if (succ.isDefined) => succ.get(v)
      case _ => throw new Exception("Incompletely defined TryTrans")
    }    
  }
}

object TryTrans {
  def apply[T, R](func: => T) = new TryTrans[T, R](func)
}