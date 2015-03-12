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
    caller.transform(
      { result => result },
      { ex =>
        ex match {
	      case ex @ PlayAjaxException(jqXHR, textStatus, errorThrown) => {
	        try {
	          val aex = read[querki.api.ApiException](jqXHR.responseText)
	          throw aex
	        } catch {
	          // The normal case -- the server sent an ApiException, which we will propagate up
	          // to the calling code:
	          case aex:querki.api.ApiException => throw aex
	          // The server sent a non-ApiException, which is unfortunate. Just display it:
	          case _:Throwable => {
	            StatusLine.showUntilChange(jqXHR.responseText)
		        throw ex	              
	          }
	        }
	      }
	      case _:Throwable => {
	        // Well, that's not good.
	        // TODO: should we have some mechanism to propagate this exception back to the server,
	        // and log it? Probably...
	        println(s"Client.interceptFailures somehow got non-PlayAjaxException $ex")
	        throw ex
	      }
        }
      }
    )
  }
  
  def makeCall(req:Request, ajax:PlayAjax):Future[String] = {
    interceptFailures(ajax.callAjax("pickledRequest" -> upickle.write(req)))
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
	      makeCall(req, controllers.ClientController.userApiRequest())       
        }
        
        case "CommonFunctions" => {
	      makeCall(req, controllers.ClientController.commonApiRequest())      
        }
        
        case _ => {
	      makeCall(req, controllers.ClientController.apiRequest(
	          DataAccess.userName, 
	          DataAccess.spaceId.underlying))
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
