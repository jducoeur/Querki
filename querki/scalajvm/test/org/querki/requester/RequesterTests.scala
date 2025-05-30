package org.querki.requester

/**
 * @author jducoeur
 */
import scala.concurrent.duration._
import akka.actor.{ActorSystem, Actor, Props}
import akka.testkit.{TestKit, ImplicitSender}
import org.scalatest.{WordSpecLike, Matchers, BeforeAndAfterAll}
 
class RequesterTests extends TestKit(ActorSystem("RequesterTests")) 
  with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll 
{ 
  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
  
  implicit val dur = 1.second
}

class Doubler extends Actor {
  def receive = {
    case n:Int => sender ! n*2
  }
}

abstract class QTestActor extends Actor with Requester {
  lazy val doubler = context.actorOf(Props(classOf[Doubler]))
  
  def doReceive:Receive
  
  def receive = doReceive
}
