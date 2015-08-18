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
  import ImportSpaceFunctions._
  
  /**
   * Begin the process of importing the given XML file.
   * 
   * This returns the path of the upload actor for this import. You should then call
   * the uploadFile() entry point (which is separate from the usual Scala API) and
   * stream the file to that actor.
   * 
   * Anyway who can create a Space is allowed to call this.
   */
  def importFromXML(name:String, size:Int):Future[String]
  
  def importFromMySQL(name:String, size:Int):Future[String]
  
  /**
   * Fetch the current state of this import.
   * 
   * @param uploader The name of the uploader, as returned by importFromXML.
   */
  def getImportProgress(uploader:String):Future[ImportProgress]
}

object ImportSpaceFunctions {
  /**
   * How far this import has come.
   * 
   * @param msg A user-visible message describing the current operation.
   * @param progress How far we are, in percent. (Approximate.)
   */
  case class ImportProgress(msg:String, progress:Int)
}
