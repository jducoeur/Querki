package querki.test.mid

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc.{Result, Session}

import querki.data.{SpaceInfo, UserInfo}

case class ClientState(testUser: TestUser, userInfo: Option[UserInfo], session: Session, spaceOpt: Option[SpaceInfo]) {
  /**
   * Evolves the current state based on the given Result Future.
   */
  def plus(resultFut: Future[Result]): Future[ClientState] = resultFut.map(result => copy(session = result.sess))
}

object ClientState {
  lazy val empty = ClientState(TestUser.Anonymous, None, new Session(), None)
  
  /**
   * This lets us inject the TestUser that these tests should be run with.
   */
  def forUser(user: TestUser) = ClientState(user, None, new Session(), None)
}
