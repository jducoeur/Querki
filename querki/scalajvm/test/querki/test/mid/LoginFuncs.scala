package querki.test.mid

import scala.concurrent.Future

import upickle.default._

import play.api.mvc.{Result, Session}
import play.api.test._
import play.api.test.Helpers._

import controllers.LoginController

import querki.data.UserInfo

case class TestUser(base: String) {
  def email = s"$base@querkitest.com"
  def password = s"Passw0rd$base"
  def handle = base
  def display = s"Test User $base"
}

case class LoginResults(result: Future[Result], userInfo: UserInfo, session: Session)

/**
 * Functions that wrap the LoginController with a higher-level API.
 */
trait LoginFuncs extends FormFuncs { self: MidTestBase =>
  private def controller = app.injector.instanceOf[LoginController]
  def loginController = controller
  
  implicit lazy val materializer = app.materializer
  
  def trySignup(email: String, password: String, handle: String, display: String): Future[Result] = {
    val request = formRequest(
      "email" -> email,
      "password" -> password,
      "handle" -> handle,
      "display" -> display
    )
    
    call(controller.signupStart(), request)    
  }
  
  def signup(email: String, password: String, handle: String, display: String): LoginResults = {
    val result = trySignup(email, password, handle, display)
    
    status(result) mustBe(OK)
    val pickledUserInfo = contentAsString(result)
    val userInfo = read[UserInfo](pickledUserInfo)
    
    LoginResults(result, userInfo, session(result))
  }
  
  def signup(user: TestUser): LoginResults = {
    signup(user.email, user.password, user.handle, user.display)
  }
  
  def tryLogin(name: String, password: String): Future[Result] = {
    val request = formRequest(
      "name" -> name,
      "password" -> password
    )
    
    controller.clientlogin()(request)
  }
  
  /**
   * Log in with the given credentials. Note that "name" may be either handle or email.
   */
  def login(name: String, password: String): LoginResults = {
    val result = tryLogin(name, password)
    
    status(result) mustBe(OK)
    val pickledUserInfo = contentAsString(result)
    val userInfoOpt = read[Option[UserInfo]](pickledUserInfo)
    assert(userInfoOpt.isDefined)
    
    LoginResults(result, userInfoOpt.get, session(result))
  }
  
  def login(user: TestUser): LoginResults = {
    login(user.handle, user.password)
  }
  
  def logout(implicit sessionIn: Session): Session = {
    val result = controller.logout()(sessionRequest)
    
    // SEE_OTHER is the official name of the Redirect function:
    status(result) must be (SEE_OTHER)
    
    session(result)
  }
}
