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
  
  val save: TestOp[ClientState] = TestOp.fetch(_.client)
  def restore(clientState: ClientState): TestOp[Unit] = TestOp.update(state => TestState.clientL.set(clientState)(state))

  /**
   * Stores the current version of the ClientState in the cache. This should generally be done just before switching to a
   * different user.
   */
  val cache: TestOp[Unit] = TestOp.update(state => TestState.clientCacheL.modify(_ + (state.client.testUser.base -> state.client))(state))
  /**
   * Switches to a different active user. This user must have previously been cached!
   */
  def switchToUser(user: TestUser): TestOp[Unit] = {
    for {
      _ <- cache
      _ <- TestOp.update(state => TestState.clientL.set(state.clientCache(user.base))(state))
    }
      yield ()
  }

  /**
    * Performs the specified operation with the specified user, and then restores the current user.
    */
  def withUser[T](user: TestUser)(op: TestOp[T]): TestOp[T] = {
    for {
      originalUser <- TestOp.fetch(_.client.testUser)
      _ <- switchToUser(user)
      t <- op
      _ <- switchToUser(originalUser)
    }
      yield t
  }

  /**
    * Performs the specified operation, and then pop the original user back out at the end.
    */
  def cachingUser[T](op: TestOp[T]): TestOp[T] = {
    for {
      originalUser <- TestOp.fetch(_.client.testUser)
      t <- op
      _ <- switchToUser(originalUser)
    }
      yield t
  }
}
