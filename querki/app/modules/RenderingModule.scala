package modules.render

import models._
import models.Thing._
import models.system._

import ql._

import querki.html.RenderSpecialization._
import querki.util._
import querki.values._

/**
 * TODO: this Module was created for _editAsPicklist, which got pulled out to EditorModule. Only reason this
 * is being left here is that it may absorb other things later. Needs to be re-enabled in Modules if so.
 */
class RenderingModule(val moduleId:Short) extends modules.Module {
  
  object MOIDs {
  }
  import MOIDs._
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  override lazy val props = Seq(
  )

}