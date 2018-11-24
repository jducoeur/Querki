package querki.test.mid

import scala.concurrent.Future

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

import upickle.default._
import autowire._

import play.api.mvc.{Result, Session}
import play.api.test._
import play.api.test.Helpers._

import controllers.LoginController

import querki.data.UserInfo
import querki.globals._
import querki.session.UserFunctions
import querki.util.SafeUrl

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
trait LoginFuncs extends FormFuncs { self: MidTestBase with ClientFuncs =>
  private def controller = app.injector.instanceOf[LoginController]
  def loginController = controller
  
  private implicit lazy val materializer = app.materializer
  
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
  
  def signupF(email: String, password: String, handle: String, display: String): StateT[IO, ClientState, LoginResults] = StateT { state =>
    IO.fromFuture(IO {
      val resultFut = trySignup(email, password, handle, display)
      val loginResultsFut = for {
        result <- resultFut
        _ <- if (result.status == OK) Future.successful(()) else throw new Exception(s"signupF got status ${result.status}!")
        pickledUserInfo <- result.contentAsStringFut
        userInfo = read[UserInfo](pickledUserInfo)
      }
        yield LoginResults(fut(result), userInfo, result.sess)
        
      resultFut.map(result => ClientState(result.sess)) zip loginResultsFut
    })
  }
  
  def signup(user: TestUser): LoginResults = {
    signup(user.email, user.password, user.handle, user.display)
  }
  
  def signupF(user: TestUser): StateT[IO, ClientState, LoginResults] =
    signupF(user.email, user.password, user.handle, user.display)
  
  def validateSignup(user: TestUser)(implicit session: Session): Unit = {
    val validateHashRaw = EmailTesting.extractValidateHash()
    val validateHash = SafeUrl.decode(validateHashRaw)
    withNsClient { c =>
      c[UserFunctions].validateActivationHash(validateHash).call().foreach { success =>
        if (!success)
          throw new Exception(s"Failed to validate user $user with hash $validateHash")
      }
    }
  }
  
  def validateSignupF(user: TestUser): StateT[IO, ClientState, Unit] = StateT { state =>
    for {
      validateHashRaw <- IO { EmailTesting.extractValidateHash()}
      validateHash = SafeUrl.decode(validateHashRaw)
      results <- IO.fromFuture(IO {
        val clnt = new NSClient()(state.session)
        val f = clnt[UserFunctions].validateActivationHash(validateHash).call()
        val fChecked = f.map { success =>
          if (!success)
            throw new Exception(s"Failed to validate user $user with hash $validateHash")          
        }
        clnt.resultSessionFut.map(ClientState(_)) zip fChecked
      })
    }
      yield results
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
  
  def loginF(name: String, password: String): StateT[IO, ClientState, LoginResults] = StateT { state =>
    IO.fromFuture(IO {
      val resultFut = tryLogin(name, password)
      val loginResultsFut = for {
        result <- resultFut
        _ <- if (result.status == OK) Future.successful(()) else throw new Exception(s"loginF got status ${result.status}!")
        pickledUserInfo <- result.contentAsStringFut
        userInfoOpt = read[Option[UserInfo]](pickledUserInfo)
        _ = if (userInfoOpt.isEmpty) throw new Exception(s"loginF didn't get userInfo when trying to log in $name!")
      }
        yield LoginResults(resultFut, userInfoOpt.get, result.sess)
        
      resultFut.map(result => ClientState(result.sess)) zip loginResultsFut
    })
  }
  
  def login(user: TestUser): LoginResults = {
    login(user.handle, user.password)
  }
  
  def loginF(user: TestUser): StateT[IO, ClientState, LoginResults] =
    loginF(user.handle, user.password)
  
  def logout(implicit sessionIn: Session): Session = {
    val result = controller.logout()(sessionRequest)
    
    // SEE_OTHER is the official name of the Redirect function:
    status(result) must be (SEE_OTHER)
    
    session(result)
  }
}
