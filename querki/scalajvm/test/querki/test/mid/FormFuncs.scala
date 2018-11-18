package querki.test.mid

import play.api.mvc.{Result, Session}
import play.api.test._
import play.api.test.Helpers._

import querki.globals._

/**
 * Functions to work with Play Forms. We actually don't do a ton of this, since Querki
 * mostly works through the Client API.
 */
trait FormFuncs { self: MidTestBase =>
  def getRequest = 
    FakeRequest()
      .copyFakeRequest(method = "GET")
  
  def sessionRequest(implicit session: Session) =
    FakeRequest()
      .copyFakeRequest(method = "GET")
      .withSession(session.data.toSeq:_*)
  
  def formRequest(formData: (String, String)*) = {
    FakeRequest()
      .copyFakeRequest(method = "POST")
      .withFormUrlEncodedBody(formData:_*)
  }
   
  def sessionFormRequest(formData: (String, String)*)(implicit session: Session) = {
    FakeRequest()
      .copyFakeRequest(method = "POST")
      .withSession(session.data.toSeq:_*)
      .withFormUrlEncodedBody(formData:_*)
  }
  
  def callByUrl(url: String): Future[Result] = {
    route(app, FakeRequest(GET, url).withHeaders("Host" -> "localhost")).getOrElse(throw new Exception(s"Unable to route to $url"))
  }
}
