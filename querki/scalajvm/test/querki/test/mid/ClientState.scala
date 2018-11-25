package querki.test.mid

import play.api.mvc.Session

import querki.data.{SpaceInfo, UserInfo}

case class ClientState(testUser: Option[TestUser], userInfo: Option[UserInfo], session: Session, spaceOpt: Option[SpaceInfo])

object ClientState {
  lazy val empty = ClientState(None, None, new Session(), None)
}
