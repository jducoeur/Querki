package querki.publication

import akka.persistence._

import org.querki.funcakka._
import org.querki.requester._

import querki.globals._

class PublicationActor(val id:OID) extends PublicationCore with RealActorCore with PersistentActor with Requester {
}
