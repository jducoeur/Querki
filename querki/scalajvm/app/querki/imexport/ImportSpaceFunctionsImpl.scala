package querki.imexport

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor._
import akka.pattern._
import akka.serialization.Serialization
import akka.util.Timeout

import querki.api._
import querki.globals._

/**
 * @author jducoeur
 */
class ImportSpaceFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with ImportSpaceFunctions {
  
  def doRoute(req:Request):Future[String] = route[ImportSpaceFunctions](this)(req)
  
  // This just kicks off the import process, by creating the Actor that will do all the
  // real work:
  def importFromXML(name:String):Future[String] = {
    val importActor = requester.context.actorOf(ImportSpaceActor.actorProps(ecology, ImportXML, name))
    
    // Now, return the fully-qualified path to that Actor:
    val path = importActor.path
    val system = requester.context.system
    val defaultAddress = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
    Future.successful(path.toStringWithAddress(defaultAddress))
  }
}
