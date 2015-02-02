package querki.editing

import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.display.Gadget
import querki.display.input.InputGadget
  
class DeriveNameCheck(valEditor:PropValueEditor)(implicit e:Ecology) extends InputGadget[dom.HTMLDivElement](e) {
  
  lazy val Editing = interface[Editing]
  lazy val PageManager = interface[querki.display.PageManager]
  
    def section = valEditor.section
    def stdThings = valEditor.stdThings
    
    def values = {
      if ($(deriveNameCheckbox.elem).prop("checked").asInstanceOf[Boolean])
        List(stdThings.types.deriveAlways.oid.underlying)
      else
        List(stdThings.types.deriveNever.oid.underlying)
    }
    
    lazy val deriveNameCheckbox = 
      Gadget(
        input(tpe:="checkbox", height:="inherit", width:="auto",
          if (section.editInfo.derivingName)
            checked:="checked",
          onchange:={ () => 
            save().foreach { response =>
              // Changing this checkbox potentially changes which Properties get loaded:
              PageManager.reload()
            } 
          }
        ))
    def doRender() = 
      div(cls:="controls",
        // Needed for save() to work:
        data("thing"):=section.thing,
        name:=Editing.propPath(stdThings.types.deriveNameProp, Some(section.thing)),
        // TODO: all this style stuff belongs in CSS:
        p(cls:="_smallSubtitle", marginBottom:=0,
          deriveNameCheckbox, " Derive the Link Name from the Display Name")
      )
      
    def hook() = {}
  }
