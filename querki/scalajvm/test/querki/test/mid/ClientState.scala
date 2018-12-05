package querki.test.mid

import play.api.mvc.{Result, Session}

import monocle.Lens
import monocle.macros.GenLens

import querki.data.{SpaceInfo, UserInfo}

/**
 * Describes the state of the "client" -- the current User, their session info, the Space they are looking at, and
 * so on.
 */
case class ClientState(std: StdThings, testUser: TestUser, userInfo: Option[UserInfo], session: Session, spaceOpt: Option[SpaceInfo]) {
  /**
   * Evolves the current state based on the given Result Future.
   */
  def plus(result: Result): ClientState = {
    // TBD: is this correct? What happens during logout? I have a feeling this isn't quite right,
    // but only the new values get sent during an ordinary call:
    val updatedSession = Session(session.data ++ result.sess.data)
    copy(session = updatedSession)
  }
}

object ClientState {  
  lazy val empty = ClientState(StdThings(Map.empty), TestUser.Anonymous, None, new Session(), None)
}
