package querki.client

import scala.concurrent.Future

import querki.globals._

import querki.comm._

class ClientImpl(e:Ecology) extends ClientEcot(e) with Client {
  
  def implements = Set(classOf[Client])
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  def interceptFailures(caller: => Future[String]):Future[String] = {
    val fut = caller
    fut.onFailure {
      case ex @ PlayAjaxException(jqXHR, textStatus, errorThrown) => {
        try {
          println(s"Trying to read ${jqXHR.responseText} as an ApiException")
          val aex = read[querki.api.ApiException](jqXHR.responseText)
          println("Deserialized")
          throw aex
        } catch {
          case aex:querki.api.ApiException => {
            println("Passing along the ApiException")
            throw aex
          }
          case _:Throwable => {
            println("Rethrowing non-ApiException")
            StatusLine.showUntilChange(jqXHR.responseText)
	        throw ex	              
          }
        }
      }
    } 
    fut
  }
  
  override def doCall(req: Request): Future[String] = {
    try {
      // TODO: handle HTTP errors from this apiRequest call. What should we do with them?
      // Put a message in the Status area?
      req.path(2) match {
        // TBD: this is kind of horrible -- somebody needs to know whether
        // this particular interface is "space-based" or "user-based". We do that here
        // because it affects the signature that we send. But it is fugly. Can we come
        // up with a higher-level abstraction of the function trait, which we use to
        // make this decision?
        case "NotificationFunctions" => {
	      controllers.ClientController.userApiRequest().callAjax("pickledRequest" -> upickle.write(req))          
        }
        
        case "CommonFunctions" => {
	      controllers.ClientController.commonApiRequest().callAjax("pickledRequest" -> upickle.write(req))          
        }
        
        case _ => {
	      interceptFailures(controllers.ClientController.apiRequest(
	          DataAccess.userName, 
	          DataAccess.spaceId.underlying).callAjax("pickledRequest" -> upickle.write(req)))
        }
      }
    } catch {
      // Note that we need to catch and report exceptions here; otherwise, they tend to get
      // lost inside Autowire:
      case (ex:Exception) => {
        println(s"Got exception in doCall: ${ex.getMessage()}")
        throw ex
      }
    }
  }

  def read[Result: upickle.Reader](p: String) = {
    try {
      upickle.read[Result](p)
    } catch {
      case ex:Exception => {
        println(s"Exception while trying to unpickle response $p: $ex")
        throw ex
      }
    }
  }
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}
