package querki.test.mid

import cats.effect.IO

import autowire._

import querki.api._
import querki.data.ThingInfo
import querki.globals._

import AllFuncs._

trait ApiFuncs {
  private def setStd(raw: Map[String, ThingInfo]): TestOp[StdThings] = TestOp { state =>
    val std = StdThings(raw)
    IO.pure((TestState.stdL.set(std)(state), std))
  }
  
  def getStd(): TestOp[StdThings] = TestOp.fetch(_.client.std)
  
  def fetchStandardThings(): TestOp[StdThings] = {
    for {
      thingMap <- TestOp.client { _[CommonFunctions].getStandardThings().call() }
      std <- setStd(thingMap)
    }
      yield std
  }
  
  def fetchStd[T](f: StdThings => T): TestOp[T] = {
    for {
      std <- fetchStandardThings()
    }
      yield f(std)
  }
}

object ApiFuncs extends ApiFuncs

case class TestPassthroughHandler(raw: Map[String, ThingInfo]) extends PassthroughHandlerBase {
  def pass(name:String):ThingInfo = raw(name)
}

case class StdThings(raw: Map[String, ThingInfo]) extends StandardThings(TestPassthroughHandler(raw))
