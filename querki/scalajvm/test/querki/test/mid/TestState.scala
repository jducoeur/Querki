package querki.test.mid

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag

import monocle.Lens
import monocle.macros.GenLens

import play.api.Application
import play.api.mvc.Result

import querki.data._

/**
 * The complete state of the test environment, which is threaded through the tests.
 * 
 * Note that the ClientState and WorldState are separate. The WorldState should be comprehensive and correct.
 * The ClientState describes a single Client's viewpoint, and you can swap Clients in the middle of testing
 * in order to represent multiple Users hitting the system at once.
 */
case class TestState(harness: HarnessInfo, client: ClientState, world: WorldState) {
  import TestState._
  
  /**
   * Higher-level wrapper around ClientState.plus, dealing with Futures.
   */
  def plus(resultFut: Future[Result]): Future[TestState] = {
    resultFut.map(result => clientL.modify(_.plus(result))(this))
  }
  
  def std = stdL.get(this)
  def testUser = testUserL.get(this)
  def withUser(user: TestUser) = testUserL.set(user)(this)
  def session = sessionL.get(this)
  
  def curSpace: SpaceTestState = {
    val spaceId = spaceOptL.get(this).getOrElse(throw new Exception(s"Trying to fetch curSpace, but the Client isn't in a Space!")).oid
    spacesL.get(this).get(spaceId).getOrElse(throw new Exception(s"Client is pointing to unknown Space $spaceId!"))
  }
}

object TestState {  
  def empty(test: MidTestBase) = TestState(HarnessInfo(test), ClientState.empty, WorldState.empty)
  
  val clientL = GenLens[TestState](_.client)
  val stdL = GenLens[TestState](_.client.std)
  val testUserL = GenLens[TestState](_.client.testUser)
  val sessionL = GenLens[TestState](_.client.session)
  val spaceOptL = GenLens[TestState](_.client.spaceOpt)
  
  val worldL = GenLens[TestState](_.world)
  val spacesL = GenLens[TestState](_.world.spaces)
}

case class HarnessInfo(test: MidTestBase) {
  lazy val app = test.app
  lazy val ecology = test.ecology
  lazy val injector = app.injector
  
  def controller[T : ClassTag] = injector.instanceOf[T]
}
