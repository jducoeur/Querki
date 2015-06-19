package querki.editing

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all._
import autowire._
import rx._

import querki.globals._

import querki.api.EditFunctions
import querki.display.{ButtonGadget, Gadget}
import querki.display.rx._
  
class PropertyEditor(val valEditor:PropValueEditor)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  lazy val prop = valEditor.propInfo
  lazy val propId = prop.oid
  
  val guts = RxGadget.of[dom.HTMLUListElement]
  guts <= ul()
  
  lazy val contentDiv = RxGadget[RxDiv].
    whenSet { g => 
      Obs(g.elemRx) {
        Gadgets.hookPendingGadgets()
      }
    }

  lazy val contentFut = {
    for {
      editInfo <- Client[EditFunctions].getPropertyEditors(propId).call()
    }
      yield new PropertySection(valEditor.section.page, s"Property $propId", editInfo.propInfos, prop, editInfo, false)
  }
  lazy val editTrigger = contentFut.foreach { section => 
    guts <= section
  }
  
  def doRender() = {
    editTrigger
    div(
      hr,
      contentDiv <= RxDiv(Rx { guts.opt().toSeq }),
      p(new ButtonGadget(ButtonGadget.Primary, "Done")({ () =>
        valEditor.propEditDone() 
      }))
    )
  }
}
