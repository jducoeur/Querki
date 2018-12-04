package querki.test.mid

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
}
