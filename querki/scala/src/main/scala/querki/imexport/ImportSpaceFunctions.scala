package querki.imexport

import scala.concurrent.Future

/**
 * Functions specifically for creating Spaces from external data files.
 * 
 * This is separate from exporting and importing data inside existing Spaces, because
 * the implementation is necessarily very different.
 * 
 * @author jducoeur
 */
trait ImportSpaceFunctions {
  /**
   * Begin the process of importing the given XML file.
   * 
   * This returns the path of the upload actor for this import. You should then call
   * the uploadFile() entry point (which is separate from the usual Scala API) and
   * stream the file to that actor.
   * 
   * Anyway who can create a Space is allowed to call this.
   */
  def importFromXML():Future[String]
}
