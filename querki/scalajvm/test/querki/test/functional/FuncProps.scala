package querki.test.functional

import models.{AnyProp, Collection}

import querki.api.commonName

/**
 * @author jducoeur
 */
trait FuncProps { this:FuncMixin =>

  sealed trait PropExtras
  case class RestrictedToModel(model:TInstance) extends PropExtras
  
  /**
   * Represents a Property.
   */
  case class TProp[TPE <: TType](
    display:String,
    coll:Collection,
    tpe:TPE,
    tid:TID = TID(""),
    extras:Seq[PropExtras] = Seq.empty) extends TThing[TProp[TPE]]
  {
    def withTID(id:String) = copy(tid = TID(id))
    
    def realProp(state:State) = {
      if (tid.length > 0)
        this
      else
        state.prop(this)
    }
    
    def setValue(v:tpe.TSetter)(thing:TThing[_], state:State):State = {
      // The asInstanceOf below is horrible, but I'm having trouble getting the compiler
      // to accept this as legit, and it is:
      tpe.setValue(thing, realProp(state).asInstanceOf[TProp[tpe.type]], v)
      state
    }
    
    /**
     * After creation, when we're in the PropertyEditor, make any needed tweaks.
     */
    def fixupProp(state:State):Unit = {
      tpe.fixupProp(this.asInstanceOf[TProp[tpe.type]])(state)
    }
  }
//  
//  object TProp {
//    def apply(real:AnyProp):TProp[_] = {
//      
//    }
//  }
  
  /**
   * The actual Simple Thing object.
   */
  object SimpleThing extends TInstance("Simple Thing", querki.basic.MOIDs.SimpleThingOID, model = null)
  
  /**
   * The Name Property.
   */
  object NameProp 
    extends TProp(commonName(_.basic.displayNameProp), ExactlyOne, TTextType, querki.basic.MOIDs.DisplayNameOID)
  
  object DefaultViewProp
    extends TProp(commonName(_.basic.defaultView), ExactlyOne, TLargeTextType, querki.basic.MOIDs.DisplayTextOID)
  
  object PageHeaderProp
    extends TProp("Page Header", Optional, TLargeTextType, querki.html.UIMOIDs.PageHeaderPropOID)
  
  object RestrictToModelProp
    extends TProp("Restrict to Model", Optional, TLinkType, querki.links.PublicMOIDs.LinkModelOID)
  
  /* **********************************************
   * Permissions
   */
  
  object MembersRole
    extends TInstance("Members", querki.security.MOIDs.MembersTagOID)

  object WhoCanExplorePerm
    extends TProp(commonName(_.roles.canExplorePerm), QSet, TLinkType, querki.security.RolesMOIDs.CanExplorePermOID)
}
