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
  def importFromXML():Future[String] = {
    val importActor = requester.context.actorOf(ImportSpaceActor.actorProps(ecology, ImportXML))
    val path = importActor.path
    val rootPath = RootActorPath(path.address)
    val fromSerialization = Serialization.serializedActorPath(importActor)
    val system = requester.context.system
    val defaultAddress = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
    val fromExtended = path.toStringWithAddress(defaultAddress)
    
    val pathStr = fromExtended
    val reconstructedPath = ActorPath.fromString(pathStr)
    
    QLog.spew(s"""toString = ${path.toString()}; 
      fromSerialization = $fromSerialization;
      fromExtended = $fromExtended;
      reconstructed = $reconstructedPath""")
      
    implicit val timeout = Timeout(2 seconds)
    
    val selection = system.actorSelection(reconstructedPath)
    loopback (selection ? Identify("dummy")) map { result =>
      println(s"Identity returned $result")
      fromSerialization
    }
  }
}
