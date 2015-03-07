package querki.editing

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._

import querki.globals._

import querki.display.{ButtonGadget, ButtonKind, Gadget}
import querki.display.rx.RxThingSelector
  
  class PropertyDetails(val valEditor:PropValueEditor)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] {
    def editInfo = valEditor.info
    def propInfo = editInfo.propInfo
    def page = valEditor.section.page
    
    // Mimic the RxSelect that the rest of the code passes in to DescriptionDiv:
    val thingSelector = 
      new RxThingSelector {
        val selectedText = Var(propInfo.displayName)
        def selectedTID = Var(propInfo.oid)
      } 
    val selectorWrapper = Var[Option[(RxThingSelector, TID)]](None)
    val descDiv = new DescriptionDiv(page, selectorWrapper)
    val propertyDescriptionDiv = descDiv.descriptionDiv
    selectorWrapper() = Some((thingSelector, propInfo.oid))
    
    def doRender() = {
      div(
        hr,
        propertyDescriptionDiv,
        if (editInfo.canEditProperty) {
          p(new ButtonGadget(ButtonKind.Primary, "Edit Property")({ valEditor.showPropEditor() }))        
        }
      )
    }
  }
