package querki.test.functional

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
  /**
   * Represents a single-line text field.
   */
  case object TTextType extends TType {
    
    type TSetter = String
    
    def tid = querki.core.MOIDs.TextTypeOID
    def display = "Text Type"
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:TSetter):Unit = {
      textField(editorId(thing, prop)).value = v
    }
  }
  
  case object TLargeTextType extends TType {
    
    type TSetter = String
    
    def tid = querki.core.MOIDs.LargeTextTypeOID
    def display = "Large Text Type"
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:TSetter):Unit = {
      textArea(editorId(thing, prop)).value = v
    }    
  }
  
  def setRestrictedToModel(prop:TProp[_])(state:State):Unit = {
    prop.extras.collect {
      case RestrictedToModel(modelProto) => {
        // HACK: for the moment, the options in the selector use OIDs, not TIDs:
        val modelId = state.thing(modelProto).oidStr
        singleSel(editorId(prop, RestrictToModelProp)).value = modelId
      }
    }
  }
  
  /**
   * Represents a Tag
   * 
   * @param modelOpt The Model that this Tag should be constrained to.
   */
  case object TTagType extends TType {
    
    type TSetter = String
    
    def tid = querki.tags.MOIDs.NewTagSetOID
    def display = "Tag Type"
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:TSetter):Unit = {
      // While Tag Sets look funky on the outside, their add-a-tag input field is
      // pretty conventional:
      textField(editorId(thing, prop)).value = v
    }
    
    override def fixupProp(prop:TProp[this.type])(state:State):Unit = {
      setRestrictedToModel(prop)(state)
    }
  }
  
  case object TLinkType extends TType {
    
    type TSetter = TThing[_]
    
    def tid = querki.core.MOIDs.LinkTypeOID
    def display = "Thing Type"
    
    def setValue(thing:TThing[_], prop:TProp[this.type], v:TSetter):Unit = {
      textField(editorId(thing, prop)).value = v.display
    }
    
    override def fixupProp(prop:TProp[this.type])(state:State):Unit = {
      setRestrictedToModel(prop)(state)
    }
  }
}