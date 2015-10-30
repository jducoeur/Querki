package querki.streaming

import akka.actor._

import org.querki.requester._

import querki.globals._

/**
 * Class for sending a stream of T to another Actor. Intended for use inside of an
 * Actor.
 * 
 * In principle, we'd rather use Akka Streaming for this, but that doesn't work
 * remotely yet, and remotely is most of the point of the exercise here.
 * 
 * @author jducoeur
 */
class StreamSender[T <% StreamElement](sendingActor:Actor, targetActor:ActorRef, elements:Iterable[T]) {
  /**
   * Actually send the elements to the target. Returns a RequestM that will resolve
   * when everything is sent.
   */
//  def send():RequestM[Unit] = {
//    
//  }
}
