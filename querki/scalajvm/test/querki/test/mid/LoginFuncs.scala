package querki.test.mid

import scala.concurrent.Future
import scala.util.{Success, Failure}

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
  
  def signup(email: String, password: String, handle: String, display: String): TestOp[LoginResults] = TestOp.fut { state =>
    val resultFut = trySignup(email, password, handle, display)
    val loginResultsFut = for {
      result <- resultFut
      _ <- if (result.status == OK) Future.successful(()) else throw new Exception(s"signupF got status ${result.status}!")
      pickledUserInfo <- result.contentAsStringFut
      userInfo = read[UserInfo](pickledUserInfo)
    }
      yield LoginResults(fut(result), userInfo, result.sess)
      
    resultFut.map(result => ClientState(result.sess)) zip loginResultsFut
  }
  
  def signup(user: TestUser): TestOp[LoginResults] =
    signup(user.email, user.password, user.handle, user.display)
  
  def validateSignup(user: TestUser): TestOp[Unit] = StateT { state =>
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
  def login(name: String, password: String): TestOp[LoginResults] = TestOp.fut { state =>
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
  }
  
  def login(user: TestUser): TestOp[LoginResults] =
    login(user.handle, user.password)
  
  def logout: TestOp[Session] = TestOp.fut { state =>
    implicit val session = state.session
    val resultFut = controller.logout()(sessionRequest)
    val checkFut = resultFut.andThen {
      // SEE_OTHER is the official name of the Redirect function:
      case Success(result) => result.status must be (SEE_OTHER)
      case Failure(ex) => throw ex
    }
    val sessionFut = checkFut.map(_.sess)
    resultFut.map(result => ClientState(result.sess)) zip sessionFut
  }
}
