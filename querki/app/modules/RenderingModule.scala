package modules.render

import models._
import models.Thing._
import models.system._

import ql._

import querki.html.RenderSpecialization._
import querki.util._
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
   * 
   * TODO: this is weirdly incestuous with HtmlRenderer. Think about how the factoring should really work.
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
    
    override def specialization(mainContext:QLContext, mainThing:Thing, 
      partialContext:QLContext, prop:Property[_,_],
      paramsOpt:Option[Seq[QLPhrase]]):Set[RenderSpecialization] = 
    {
      // This is basically saying "if there is one parameter, and it is the token 'withAdd'"
      // TODO: all of this should go behind a better-built parameter wrapper.
      val hasAddOpt = for (
        params <- paramsOpt;
        if (params.length > 0);
        param = params(0);
        QLCall(addName, _, _, _) = param.ops(0);
        if (addName.toLowerCase() == "withadd")
          )
        yield true
        
      hasAddOpt.map(_ => Set(PickList, WithAdd)).getOrElse(Set(PickList))
    }
  }
  
  override lazy val props = Seq(
    editAsPicklistMethod
  )

}