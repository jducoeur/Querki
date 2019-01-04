package querki.test.mid

import scala.concurrent.Future
import scala.util.{Success, Failure}

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

import upickle.default._
import autowire._

import org.scalatest._
import Matchers._

import play.api.Application
import play.api.mvc.{Result, Session}
import play.api.test._
import play.api.test.Helpers._

import controllers.LoginController

import querki.data.{SpaceInfo, UserInfo}
import querki.globals._
import querki.session.UserFunctions
import querki.util.SafeUrl

import AllFuncs._

case class TestUser(base: String) {
  // "Safe" version of the name, suitable for email and display:
  lazy val safe = base.toLowerCase().filter(_.isLetter)
  
  lazy val email = s"$safe@querkitest.com"
  lazy val password = s"Passw0rd$base"
  lazy val handle = safe
  lazy val display = s"Test User $base"
}
object TestUser {
  val Anonymous = TestUser("")
}

case class LoginResults(result: Future[Result], userInfo: UserInfo, session: Session)

/**
 * Functions that wrap the LoginController with a higher-level API.
 */
trait LoginFuncs {
  private def controller(implicit app: Application) = app.injector.instanceOf[LoginController]
  private def mat(implicit app: Application) = app.materializer
    
  def trySignup(email: String, password: String, handle: String, display: String)(implicit app: Application): Future[Result] = {
    val request = formRequest(
      "email" -> email,
      "password" -> password,
      "handle" -> handle,
      "display" -> display
    )
    
    implicit val m = mat
    call(controller.signupStart(), request)
  }
  
  def signup: TestOp[LoginResults] = TestOp.fut { state =>
    implicit val app = state.harness.app
    implicit val m = mat
    val user = state.testUser
    val resultFut = trySignup(user.email, user.password, user.handle, user.display)
    val loginResultsFut = for {
      result <- resultFut
      _ <- if (result.status == OK) Future.successful(()) else throw new Exception(s"signupF got status ${result.status}!")
      pickledUserInfo <- result.contentAsStringFut
      userInfo = read[UserInfo](pickledUserInfo)
    }
      yield LoginResults(fut(result), userInfo, result.sess)
      
    state.plus(resultFut) zip loginResultsFut
  }
  
  def validateSignup: TestOp[Unit] = StateT { state =>
    implicit val ecology = state.harness.ecology
    val user = state.testUser
    for {
      validateHashRaw <- IO { EmailTesting.extractValidateHash()}
      validateHash = SafeUrl.decode(validateHashRaw)
      results <- IO.fromFuture(IO {
        val clnt = new NSClient(state.harness, state.session)
        val f = clnt[UserFunctions].validateActivationHash(validateHash).call()
        val fChecked = f.map { success =>
          if (!success)
            throw new Exception(s"Failed to validate user $user with hash $validateHash")          
        }
        state.plus(clnt.resultFut) zip fChecked
      })
    }
      yield results
  }
  
  def tryLogin(name: String, password: String)(implicit app: Application): Future[Result] = {
    val request = formRequest(
      "name" -> name,
      "password" -> password
    )
    
    controller.clientlogin()(request)
  }

  /**
   * Pokes the specified TestUser into the current TestState.
   */
  def setUser(user: TestUser): TestOp[Unit] = TestOp { state =>
    IO.pure((state.withUser(user), ()))
  }
  
  /**
   * Creates a new User, and initializes the ClientState for it. Usually the beginning of a test.
   */
  def newUser(user: TestUser): TestOp[LoginResults] = {
    for {
      // If there was a previous active user, save their state for later restore:
      _ <- ClientState.cache
      _ <- setUser(user)
      _ <- signup
      _ <- validateSignup
      loginResults <- login
      _ = loginResults.session("username") should be (user.handle)
    }
      yield loginResults
  }
  
  /**
   * Log in with the given credentials. Note that "name" may be either handle or email.
   */  
  def login: TestOp[LoginResults] = TestOp.fut { state =>
    implicit val app = state.harness.app
    implicit val m = mat
    val user = state.testUser
    val resultFut = tryLogin(user.handle, user.password)
    val loginResultsFut = for {
      result <- resultFut
      _ <- if (result.status == OK) Future.successful(()) else throw new Exception(s"loginF got status ${result.status}!")
      pickledUserInfo <- result.contentAsStringFut
      userInfoOpt = read[Option[UserInfo]](pickledUserInfo)
      _ = if (userInfoOpt.isEmpty) throw new Exception(s"loginF didn't get userInfo when trying to log in ${user.handle}!")
    }
      yield LoginResults(resultFut, userInfoOpt.get, result.sess)
      
    state.plus(resultFut) zip loginResultsFut
  }
  
  def logout: TestOp[Session] = TestOp.fut { state =>
    implicit val app = state.harness.app
    implicit val session = state.session
    val resultFut = controller.logout()(sessionRequest)
    val checkFut = resultFut.andThen {
      // SEE_OTHER is the official name of the Redirect function:
      case Success(result) => result.status should be (SEE_OTHER)
      case Failure(ex) => throw ex
    }
    val sessionFut = checkFut.map(_.sess)
    state.plus(resultFut) zip sessionFut
  }
  
  /**
   * Process the given invitation (which should be for the current user) to join the specified Space.
   */
  def acceptInvite(inviteRaw: String, spaceInfo: SpaceInfo, expectFailure: Boolean = false): TestOp[Unit] = TestOp.fut { state =>
    implicit val app = state.harness.app
    implicit val session = state.session
    implicit val m = mat
    
    val invite = SafeUrl.decode(inviteRaw)
    
    val request = formRequest(
      "invite" -> invite
    )
    val resultFut = call(controller.handleInvite2(spaceInfo.ownerHandle, spaceInfo.oid.underlying), request)
    val inviteResultFut = for {
      result <- resultFut
      _ <- if (result.status == OK) Future.successful(()) else throw new Exception(s"acceptInvite got status ${result.status}!")
      pickledUserInfo <- result.contentAsStringFut
      _ = 
        if (expectFailure)
          assert(pickledUserInfo.isEmpty)
        else {
          // Normally, make sure there is a legal user info here:
          assert (!pickledUserInfo.isEmpty, s"acceptInvite got an empty return value!")
          read[UserInfo](pickledUserInfo)
        }
    }
      yield ()
      
    inviteResultFut.map((state, _))
  }
}

object LoginFuncs extends LoginFuncs
