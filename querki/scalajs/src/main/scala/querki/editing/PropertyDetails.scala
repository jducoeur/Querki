package querki.editing

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import org.querki.gadgets._

import querki.globals._

import querki.display.{ButtonGadget}
import querki.display.rx.RxThingSelector
  
class PropertyDetails(val valEditor:PropValueEditor)(implicit val ecology:Ecology) extends Gadget[dom.html.Div] {
  def editInfo = valEditor.info
  def propInfo = editInfo.propInfo
  def page = valEditor.section.page
  
  // Mimic the RxSelect that the rest of the code passes in to DescriptionDiv:
  val thingSelector = 
    new RxThingSelector {
      val selectedText = Var(propInfo.displayName)
      def selectedTID = Var(propInfo.oid)
    } 
  val selectorWrapper = Var[Option[(RxThingSelector, TID)]](Some((thingSelector, propInfo.oid)))
  
  lazy val desc = new DescriptionDiv(page, selectorWrapper)
  
  def doRender() = {
    div(
      hr,
      desc,
      if (editInfo.canEditProperty) {
        p(new ButtonGadget(ButtonGadget.Primary, "Edit Property")({ () => valEditor.showPropEditor() }))        
      }
    )
  }
}
