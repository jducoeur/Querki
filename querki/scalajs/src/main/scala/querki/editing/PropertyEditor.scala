package querki.editing

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all._
import autowire._
import rx._

import querki.globals._

import querki.api.EditFunctions
import querki.display.{ButtonGadget, ButtonKind, Gadget}
import querki.display.rx.RxDiv
  
class PropertyEditor(val valEditor:PropValueEditor)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  lazy val Client = interface[querki.client.Client]
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  
    lazy val prop = valEditor.propInfo
    lazy val propId = prop.oid
    lazy val empty = ul()
    lazy val guts = Var[Gadget[dom.HTMLUListElement]](empty)
    lazy val contentDiv = RxDiv(Rx {Seq(guts())})
    lazy val contentFut = {
      for {
        editInfo <- Client[EditFunctions].getPropertyEditors(propId).call()
      }
        yield new PropertySection(valEditor.section.page, s"Property $propId", editInfo.propInfos, prop, editInfo, valEditor.stdThings, false)
    }
    lazy val editTrigger = contentFut.foreach { section => 
      guts() = section
      hookWhenDone
    }
    lazy val hookWhenDone = Obs(contentDiv.elemRx) {
      InputGadgets.hookPendingGadgets()      
    }
    
    def doRender() = {
      editTrigger
      div(
        hr,
        contentDiv,
        p(new ButtonGadget(ButtonKind.Primary, "Done")({ 
          valEditor.propEditDone() 
        }))
      )
    }
  }
