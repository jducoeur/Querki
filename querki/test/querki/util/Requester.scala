package querki.util

import scala.concurrent.duration._

import akka.actor._
import akka.pattern.AskTimeoutException
import akka.testkit.{TestKit, ImplicitSender}
import akka.util.Timeout

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

case class Start(trans:ActorRef)
case class Stringify(num:Int)
case class Stringified(str:String)
case object UnknownMessage

case class StartMessy(trans:ActorRef)

class Transformer extends Actor {  
  def receive = {
    case Stringify(num) => sender ! Stringified(num.toString)
  }
}

class MyRequester extends Actor with Requester with ActorLogging {
  
  var results = ""
  
  def receive = {
    case Start(trans) => {
      trans.request(Stringify(4)) {
        case Stringified(str) => {
          results += str
          sender ! results
        }
      }
    }
    
    case StartMessy(trans) => {
      trans.request(Stringify(5)) {
        case Stringified(str) => {
          results += str
          
          trans.request(Stringify(6)) {
            case Stringified(str) => {
              results += str
              
              trans.request(UnknownMessage) {
                case e:AskTimeoutException => {
                  sender ! results
                }
                case _ => throw new Exception("Blah! Shouldn't have gotten here!")
              }
            }
          }
        }
      }
    }
  }
}

  // TODO: these tests are hanging when run from Play. Why?
//class RequesterTests extends TestKit(ActorSystem("AltimeterSpec"))
//  with ImplicitSender
//  with WordSpec
//  with ShouldMatchers
//  with BeforeAndAfterAll 
//{
//  "Requester" should {
//    "handle a simple roundtrip" in {
//      val req = system.actorOf(Props[MyRequester])
//      val trans = system.actorOf(Props[Transformer])
//      req ! Start(trans)
//      expectMsg("4")
//    }
//    
//    "handle messy timeouts" in {
//      val req = system.actorOf(Props[MyRequester])
//      val trans = system.actorOf(Props[Transformer])
//      req ! StartMessy(trans)
//      expectMsg("56")      
//    }
//  }
//}