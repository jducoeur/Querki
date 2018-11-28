package querki.test.mid

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc.{Result, Session}

import monocle.Lens
import monocle.macros.GenLens

import querki.data.{SpaceInfo, UserInfo}

/**
 * Describes the state of the "client" -- the current User, their session info, the Space they are looking at, and
 * so on.
 */
case class ClientState(testUser: TestUser, userInfo: Option[UserInfo], session: Session, spaceOpt: Option[SpaceInfo]) {
  /**
   * Evolves the current state based on the given Result Future.
   */
  def plus(result: Result): ClientState = copy(session = result.sess)
}

object ClientState {  
  lazy val empty = ClientState(TestUser.Anonymous, None, new Session(), None)
  
  val testUserL = GenLens[ClientState](_.testUser)
  val sessionL = GenLens[ClientState](_.session)
}

/**
 * The state of the world -- the Spaces and Things in them -- from the test harness' point of view.
 */
case class WorldState()

object WorldState {
  lazy val empty = WorldState()
}

/**
 * The complete state of the test environment, which is threaded through the tests.
 * 
 * Note that the ClientState and WorldState are separate. The WorldState should be comprehensive and correct.
 * The ClientState describes a single Client's viewpoint, and you can swap Clients in the middle of testing
 * in order to represent multiple Users hitting the system at once.
 */
case class TestState(client: ClientState, world: WorldState) {
  import TestState._
  
  /**
   * Higher-level wrapper around ClientState.plus, dealing with Futures.
   */
  def plus(resultFut: Future[Result]): Future[TestState] = {
    resultFut.map(result => clientL.modify(_.plus(result))(this))
  }
  
  def testUser = testUserL.get(this)
  def withUser(user: TestUser) = testUserL.set(user)(this)
  def session = sessionL.get(this)
}

object TestState {  
  lazy val empty = TestState(ClientState.empty, WorldState.empty)
  
  val clientL = GenLens[TestState](_.client)
  val testUserL = (clientL composeLens ClientState.testUserL)
  val sessionL = (clientL composeLens ClientState.sessionL)
}
