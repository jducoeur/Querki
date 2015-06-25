package querki.imexport

import querki.globals._
import querki.session.{AutowireApiImpl, AutowireParams}

/**
 * Implementation of the public API for export/import functionality.
 * 
 * @author jducoeur
 */
class ImexportFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with ImexportFunctions  {
  lazy val Imexport = interface[Imexport]
  
  def exportSpace:String = {
    Imexport.exportSpace(rc)(state)
  }
}