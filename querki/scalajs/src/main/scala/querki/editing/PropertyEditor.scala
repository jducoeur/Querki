package querki.editing

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.squery.Focusable._
import org.querki.gadgets._

import querki.globals._

import querki.display.{ButtonGadget}
import querki.display.rx._
  
class PropertyEditor(val valEditor:PropValueEditor)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  lazy val prop = valEditor.propInfo
  lazy val propId = prop.oid
  
  val guts = QGadgetRef.of[dom.HTMLUListElement].whenSet { x => Gadgets.hookPendingGadgets() }

  def doRender() = {
    val result = div(
      hr,
      guts <= ul(),
      p(new ButtonGadget(ButtonGadget.Primary, "Done")({ () =>
        valEditor.propEditDone() 
      }))
    )
    
    for {
      editInfo <- Client[EditFunctions].getPropertyEditors(propId).call()
      section = new PropertySection(valEditor.section.page, s"Property $propId", editInfo.propInfos, prop, editInfo, false)
    } {
      // Note that PropertySection is, at heart, a Gadget[UList], so this is legal. This version of
      // the guts will get swapped in when we get the PropertyEditors:
      guts <= section
      section.focus()
    }
      
    result
  }
}
