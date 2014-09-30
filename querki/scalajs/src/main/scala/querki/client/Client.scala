package querki.client

import scala.concurrent.Future

import querki.globals._

import querki.api.ClientApis
import querki.comm._

class ClientImpl(e:Ecology) extends ClientEcot(e) with Client {
  
  def implements = Set(classOf[Client])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  override def doCall(req: Request): Future[String] = {
    controllers.ClientController.apiRequest(
        DataAccess.userName, 
        DataAccess.spaceId, 
        ClientApis.ThingFunctionsId, 
        req.path.mkString("/"), 
        upickle.write(req.args)).callAjax()
  }

  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}
