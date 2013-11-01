package modules.render

import models._
import models.Thing._
import models.system._

import ql._

import querki.html.RenderSpecialization._
import querki.values._

class RenderingModule(val moduleId:Short) extends modules.Module {
  
  object MOIDs {
    val EditAsPickListOID = moid(1)
  }
  import MOIDs._
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  /**
   * This is probably badly factored -- in the long run, I suspect this should actually be a param to _edit instead.
   * But this will do to start.
   */
  lazy val editAsPicklistMethod = new EditMethodBase(EditAsPickListOID, 
    toProps(
      setName("_editAsPickList"),
      PropSummary("Edits a Tag or Link Set as a Pick List"),
      PropDetails("""This is broadly similar to [[_edit._self]], but displays in a way that is sometimes more useful.
          |
          |To use _editAsPickList, your set must have a Link Model set. This displays all known instances of that Link Model
          |as a checklist, and allows you to decide what is in or out simply by checking things in the list.""".stripMargin),
      AppliesToKindProp(Kind.Property)
    )) 
  {
    // TODO: this is stolen directly from _edit, and should probably be refactored:
    def cantEditFallback(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      params:Option[Seq[QLPhrase]]):QValue = {
        // This user isn't allowed to edit, so simply render the property in its default form.
        // For more control, user _editOrElse instead.
        prop.qlApply(mainContext, params)    
    }  
    
    override val specialization:RenderSpecialization = PickList
  }
  
  override lazy val props = Seq(
    editAsPicklistMethod
  )

}