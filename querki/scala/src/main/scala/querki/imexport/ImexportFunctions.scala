package querki.imexport

/**
 * @author jducoeur
 */
trait ImexportFunctions {
  /**
   * Export the current Space as XML.
   * 
   * Only a Manager may call this.
   */
  def exportSpace():String
}
