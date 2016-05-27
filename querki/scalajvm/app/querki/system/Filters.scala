package querki.system

import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._

import play.api.http.HttpFilters

import querki.globals._

class Filters extends HttpFilters {
  val filters = Seq(LoggingFilter)
}

/**
 * General filter that permits extensive logging of all requests. Adapted from example found in
 * Play documentation:
 * 
 *   http://www.playframework.com/documentation/2.1.1/ScalaHttpFilters
 *   
 * Basically, when querki.test.logAllRequests is set to true, we show the requests, the results,
 * and how long they took.
 * 
 * TODO: this probably must be modified for production use before GA, simply for security reasons:
 * it leaks too much user info into the logs. The request side *may* be okay, but the result side
 * certainly is not.
 */
object LoggingFilter extends Filter {
  
  lazy val logAllRequests = Config.getBoolean("querki.test.logAllRequests", false)
  
  // TODO: this is, obviously, insufficient even to a loaded server, much less a full cluster.
  // Come up with a better mechanism for deriving a consistent ID that threads through the
  // whole logic of a request!
  var nextRequestId:Int = 0
  
  def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader):Future[Result] = {
    if (logAllRequests) {
      // TODO: this isn't sufficiently atomic. See above comment: we need a proper mechanism for
      // this eventually.
      nextRequestId += 1
      val reqId = nextRequestId
      // Log the beginning of the request...
      QLog.info(s"+++ $reqId -- ${rh.method} ${rh.uri} from ${rh.session.get(Security.username)}...")
      val start = System.currentTimeMillis

      def logTime(result: Result): Result = {
        val time = System.currentTimeMillis - start
        // ... and (possibly asynchronously) the end...
        QLog.info(s"... $reqId -- took ${time}ms and returned $result")
        result.withHeaders("Request-Time" -> time.toString)
      }

      try {
        next(rh).map { res =>
          logTime(res)
        }
      } catch {
        case ex:Exception => {
          QLog.error(s"!!! $reqId -- threw Exception", ex)
          throw ex
        }
      }
    } else {
      // Normal pathway, if the config flag is turned off:
      next(rh)
    }
  }
}
