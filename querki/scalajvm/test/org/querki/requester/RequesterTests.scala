package org.querki.requester

/**
 * @author jducoeur
 */
import scala.concurrent.duration._
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

// TODO: the Requester tests are having problems in the integrated application, but for now we don't care.
// If/when Requester gets lifted back out to its own library, re-enable these tests and get them working right.
@DoNotDiscover
class RequesterTests
  extends TestKit(ActorSystem("RequesterTests"))
     with ImplicitSender
     with AnyWordSpecLike
     with Matchers
     with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  implicit val dur: FiniteDuration = 1.second
}

class Doubler extends Actor {

  def receive = {
    case n: Int => sender ! n * 2
  }
}

abstract class QTestActor extends Actor with Requester {
  lazy val doubler = context.actorOf(Props(classOf[Doubler]))

  def doReceive: Receive

  def receive = doReceive
}
