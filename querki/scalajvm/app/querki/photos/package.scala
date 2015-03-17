package querki

import scala.concurrent.Future

import akka.actor.ActorRef

import querki.ecology._

package object photos {
  trait Photos extends EcologyInterface {
    /**
     * Begins the process of processing a newly-uploaded photograph.
     * 
     * This will create a worker Actor that deals with the actual processing and uploading to storage.
     * The calling code should then send messages to that Actor, with the actual body of the image.
     */
    def createWorker(mimeType:Option[String]):Future[ActorRef]
  }
}
