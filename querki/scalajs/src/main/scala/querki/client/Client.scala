package querki.client

import scala.concurrent.Future

import querki.globals._

import querki.comm._

class ClientImpl(e:Ecology) extends ClientEcot(e) with Client {
  
  def implements = Set(classOf[Client])
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  override def doCall(req: Request): Future[String] = {
    try {
      // TODO: handle HTTP errors from this apiRequest call. What should we do with them?
      // Put a message in the Status area?
      controllers.ClientController.apiRequest(
          DataAccess.userName, 
          DataAccess.spaceId, 
          upickle.write(req)).callAjax()
    } catch {
      // Note that we need to catch and report exceptions here; otherwise, they tend to get
      // lost inside Autowire:
      case (ex:Exception) => {
        println(s"Got exception in doCall: ${ex.getMessage()}")
        throw ex
      }
    }
  }

  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}
