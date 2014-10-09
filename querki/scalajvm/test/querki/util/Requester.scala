package querki.util

import scala.concurrent.duration._

import akka.actor._
import akka.pattern.AskTimeoutException
import akka.testkit.{TestKit, ImplicitSender}
import akka.util.Timeout

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
