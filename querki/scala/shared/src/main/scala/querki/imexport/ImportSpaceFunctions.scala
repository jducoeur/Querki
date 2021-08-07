package querki.imexport

import scala.concurrent.Future

import querki.data.SpaceInfo

/**
 * Functions specifically for creating Spaces from external data files.
 *
 * This is separate from exporting and importing data inside existing Spaces, because
 * the implementation is necessarily very different.
 *
 * @author jducoeur
 */
trait ImportSpaceFunctions {
  import ImportSpaceFunctions._

  /**
   * Begin the process of importing the given XML file.
   *
   * This returns the path of the upload actor for this import. You should then call
   * the uploadFile() entry point (which is separate from the usual Scala API) and
   * stream the file to that actor.
   *
   * After uploadFile completes, you should call getImportProgress() periodically (once a
   * second is recommended). This is an important part of the protocol -- while the
   * exact interval doesn't matter, the client needs to pay attention to when things
   * are complete, and acknowledge that it knows that.
   *
   * Anyway who can create a Space is allowed to call this.
   */
  def importFromXML(
    name: String,
    size: Int
  ): Future[String]

  def importFromMySQL(
    name: String,
    size: Int
  ): Future[String]

  /**
   * Fetch the current state of this import.
   *
   * @param uploader The name of the uploader, as returned by importFromXML.
   */
  def getImportProgress(uploader: String): Future[ImportProgress]

  /**
   * The client should call acknowledgeComplete after it receives an ImportProgress
   * with the spaceInfo set. This tells the server that the client knows the
   * upload is done, and that it can shut down and clean up.
   */
  def acknowledgeComplete(uploader: String): Unit
}

object ImportSpaceFunctions {

  /**
   * How far this import has come.
   *
   * @param msg A user-visible message describing the current operation.
   * @param progress How far we are, in percent. (Approximate.)
   * @param spaceInfo Iff the operation has completed successfully, the info about the newly-built Space.
   * @param failed Set to true iff the import has crashed, in which case msg describes the error.
   */
  case class ImportProgress(
    msg: String,
    progress: Int,
    spaceInfo: Option[SpaceInfo],
    failed: Boolean = false
  )
}
