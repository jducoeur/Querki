package querki.test.mid

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

import autowire._

import play.api.mvc.Session

import querki.data.SpaceInfo
import querki.globals._
import querki.session.UserFunctions

case class ClientState(session: Session)

/**
 * Provides functions for creating and manipulating Spaces.
 */
trait SpaceFuncs { self: ClientFuncs =>
  def createSpace(name: String)(implicit session: Session): SpaceInfo = {
    withNsClient { client =>
      client[UserFunctions].createSpace(name, None).call().waitFor()
    }
  }
  
  def createSpaceF(name: String): StateT[IO, ClientState, SpaceInfo] = StateT { state =>
    IO.fromFuture(IO {
      val clnt = new NSClient()(state.session)
      val f: Future[SpaceInfo] = clnt[UserFunctions].createSpace(name, None).call()
      clnt.resultSessionFut.map(ClientState(_)) zip f
    })
  }
}
