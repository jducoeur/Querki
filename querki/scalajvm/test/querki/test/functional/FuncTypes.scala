package querki.test.functional

import models.PType

/**
 * @author jducoeur
 */
trait FuncTypes { this:FuncMixin =>
  
  /**
   * Enumeration of the PTypes that we can test.
   */
  trait TType {
    
    type TSetter
    
    def tid:TID
    def display:String
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:TSetter):Unit
    
    def fixupProp(prop:TProp[this.type])(state:State):Unit = {}
  }
  
  abstract class TSystemType[T](pt:PType[T]) extends TType {
    def tid = pt.id
    def display = pt.displayName
  }
  
  trait StringSetter { this:TType =>
    type TSetter = String
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:String):Unit = {
      // Don't just set textField.value -- we need to be sure to tab out of here. This is because,
      // in the TTagType, it's going to display the menu of options (or the "not found" box) until
      // we exit this field, and that box can block the button we need next:
      click on editorId(thing, prop)
      enter(s"""$v\u0009""")
    }    
  }
  
  lazy val TTextType = new TSystemType(ICore.TextType) with StringSetter
  lazy val TLargeTextType = new TSystemType(ICore.LargeTextType) {
    type TSetter = String
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:TSetter):Unit = {
      textArea(editorId(thing, prop)).value = v
    }    
  }
  
  /**
   * Utility function for setting up Link and Tag Properties.
   */
  trait ModelRestrictions { this:TType =>
    override def fixupProp(prop:TProp[this.type])(state:State):Unit = {
      prop.extras.collect {
        case RestrictedToModel(modelProto) => {
          // HACK: for the moment, the options in the selector use OIDs, not TIDs:
          val modelId = state.thing(modelProto).oidStr
          singleSel(editorId(prop, RestrictToModelProp)).value = modelId
        }
      }
    }
  }
  
  /**
   * Represents a Tag
   */
  lazy val TTagType = new TSystemType(ITags.NewTagSetType) with StringSetter with ModelRestrictions
  lazy val TLinkType = new TSystemType(ICore.LinkType) with ModelRestrictions {
    type TSetter = TThing[_]
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:TSetter):Unit = {
      textField(editorId(thing, prop)).value = v.display
    }
  }
}