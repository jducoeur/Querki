package querki.test.mid

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._

import autowire._

import play.api.mvc.Session

import querki.api._
import querki.data.ThingInfo
import querki.globals._

import AllFuncs._

trait ApiFuncs {
  /**
   * Fetch the StandardThings.
   * 
   * This is a constant data structure, so we simply have it cached in the TestState.
   */
  val getStd: TestOp[StdThings] = TestOp.fetch(_.client.std)
  
  /**
   * Initialize the TestState.
   * 
   * This involves fetching the StandardThings, same as the Client does at startup. It might
   * later involve other operations.
   */
  def initState: IndexedStateT[IO, PreInitialState, TestState, Unit] = IndexedStateT { preState =>
    val client = new NSClient(preState.harness, new Session())
    val stateFut = for {
      stdThingMap <- client[CommonFunctions].getStandardThings().call()
    }
      yield TestState(
        preState.harness,
        ClientState(StdThings(stdThingMap), TestUser.Anonymous, None, Session(client.resultFut.value.get.get.sess.data), None),
        WorldState.empty
      )

    IO.fromFuture { IO { stateFut zip fut(()) } }
  }
  
  def fetchStd[T](f: StdThings => T): TestOp[T] = {
    for {
      std <- getStd
    }
      yield f(std)
  }
}

object ApiFuncs extends ApiFuncs

case class TestPassthroughHandler(raw: Map[String, ThingInfo]) extends PassthroughHandlerBase {
  def pass(name:String):ThingInfo = raw(name)
}

case class StdThings(raw: Map[String, ThingInfo]) extends StandardThings(TestPassthroughHandler(raw))
