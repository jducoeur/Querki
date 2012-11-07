package models

import akka.actor._
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout
import play.api.Configuration
import play.api.Play
import play.api.Play.current
import play.api.libs.concurrent._
import play.Configuration

sealed trait SpaceMessage
case class Load() extends SpaceMessage

class Space extends Actor {

  def receive = {
    case Load => {}
  }
}

sealed trait SpaceMgrMsg
case class GetSpace(id:String) extends SpaceMgrMsg
// TEMP:
case class SaySomething(something:String) extends SpaceMgrMsg

class SpaceManager extends Actor {
  
  var counter = 0
  
  // TEMP:
  val replyMsg = Play.configuration.getString("querki.test.replyMsg").getOrElse("MISSING REPLY MSG!")
  
  def receive = {
    // TEMP:
    case msg:SaySomething => {
      counter += 1
      sender ! msg.something + replyMsg + counter
    }
    case GetSpace => {}
  }
  
}

object SpaceManager {
  // I don't love having to hold a static reference like this, but Play's statelessness
  // probably requires that:
  lazy val ref = Akka.system.actorOf(Props[SpaceManager], name="SpaceManager")
  
  // This is probably over-broad -- we're going to need the timeout to push through to
  // the ultimate callers.
  implicit val timeout = Timeout(5 seconds)
  
  // Send a message to the SpaceManager, expecting a return of type A to be
  // passed into the callback. This wraps up the messy logic to go from a
  // non-actor-based Play environment to the SpaceManager. We'll likely
  // generalize it further eventually.
  //
  // Type A is the response we expect to get back from the message, which will
  // be sent to the given callback.
  //
  // Type B is the type of the callback. I'm a little surprised that this isn't
  // inferred -- I suspect I'm doing something wrong syntactically.
  def ask[A,B](msg:SpaceMgrMsg)(cb: A => B)(implicit m:Manifest[A]):Promise[B] = {
    (ref ? msg).mapTo[A].map(cb).asPromise
  }
}