package querki.imexport

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor._
import akka.pattern._
import akka.serialization.Serialization
import akka.util.Timeout

import models.AsName
import querki.api._
import querki.globals._

/**
 * @author jducoeur
 */
class ImportSpaceFunctionsImpl(info: AutowireParams)(implicit e: Ecology)
  extends AutowireApiImpl(info, e)
     with ImportSpaceFunctions {

  import ImportSpaceFunctions._

  def doRoute(req: Request): Future[String] = route[ImportSpaceFunctions](this)(req)

  lazy val SpaceOps = interface[querki.spaces.SpaceOps]

  // This just kicks off the import process, by creating the Actor that will do all the
  // real work:
  def importFromType(
    typ: ImportDataType,
    name: String,
    size: Int
  ): Future[String] = {
    // TODO: the mainIdentity thing here is a weird smell. The implication is that this function should take as
    // a parameter the identity that I want to create this Space under, since Spaces are owned by Identities,
    // not by Users.
    SpaceOps.spaceExists(user.mainIdentity.id, AsName(name)).map { exists =>
      if (exists)
        throw new SpaceExistsException(name)
      else {
        val importActor = requester.context.actorOf(ImportSpaceActor.actorProps(ecology, typ, name, size))

        // Now, return the fully-qualified path to that Actor:
        val path = importActor.path
        val system = requester.context.system
        val defaultAddress = system.asInstanceOf[ExtendedActorSystem].provider.getDefaultAddress
        path.toStringWithAddress(defaultAddress)
      }
    }
  }

  def importFromXML(
    name: String,
    size: Int
  ): Future[String] = importFromType(ImportXML, name, size)

  def importFromMySQL(
    name: String,
    size: Int
  ): Future[String] = importFromType(ImportMySQL, name, size)

  def getImportProgress(path: String): Future[ImportProgress] = {
    val selection = context.system.actorSelection(path)
    selection.requestFor[ImportProgress](ImportSpaceActor.GetProgress)
  }

  def acknowledgeComplete(path: String): Unit = {
    val selection = context.system.actorSelection(path)
    selection ! ImportSpaceActor.CompletionAcknowledged
  }
}
