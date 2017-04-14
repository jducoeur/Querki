package querki.publication

import querki.ecology._
import querki.globals._

import PublicationEvents._

class PublicationEcot(e:Ecology) extends QuerkiEcot(e) {
  
  override def persistentMessages = persist(68,
    (classOf[PublishEvent] -> 100)
  )

}