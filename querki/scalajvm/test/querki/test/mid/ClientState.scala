package querki.test.mid

import play.api.mvc.{Result, Session}
import monocle.Lens
import monocle.macros.GenLens
import org.scalactic.source.Position
import querki.data.{TID, SpaceInfo, UserInfo}

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
  def switchToUser(user: TestUser)(implicit pos: Position): TestOp[Unit] = {
    for {
      _ <- cache
      userState <- fetchUserState(user)
      _ <- TestOp.update(state => TestState.clientL.set(userState)(state))
    }
      yield ()
  }

  def fetchUserState(user: TestUser)(implicit pos: Position): TestOp[ClientState] = {
    for {
      cache <- TestOp.fetch(_.clientCache)
      baseName = user.base
      state <-
        cache.get(baseName) match {
          case Some(state) => TestOp.pure(state)
          case None => TestOp.error(new Exception(s"Unable to find state for a user named '$baseName'!"))
        }
    }
      yield state
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

  /**
    * Fetch the TID of a user who has previously been created in this test.
    */
  def userId(user: TestUser): TestOp[TID] = {
    TestOp.fetch { state =>
      val oid = TestState.clientL.get(state).userInfo
        .orElse(TestState.clientCacheL.get(state).apply(user.base).userInfo)
        .getOrElse(throw new Exception(s"Didn't find expected user $user!"))
        .oid
      TID(oid)
    }
  }
}
