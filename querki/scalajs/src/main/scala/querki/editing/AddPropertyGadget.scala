package querki.editing

import org.scalajs.dom.{raw => dom}
import rx._
import rx.ops._
import scalatags.JsDom.all._

import querki.globals._

import querki.data.{TID => _TID, _}
import querki.display.{AfterLoading, ButtonGadget, Gadget, WrapperDiv}
import ButtonGadget._
import querki.display.rx.RxGadget

class AddPropertyGadget(page:ModelDesignerPage, thing:ThingInfo)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val mainDiv = RxGadget[WrapperDiv]
  val initButton = RxGadget[ButtonGadget]
  
  lazy val cancelButton = new ButtonGadget(Normal, "Cancel")({ () => reset() })
  
  def reset() = {
    addExistingGadget.map(_.reset())
    createNewGadget.map(_.reset())
    mainDiv.replaceContents(initButton.rendered, true)
  }
  
  // This is a bit boilerplatey, but we're trying not to evaluate addExisting unnecessarily
  lazy val addExisting = AfterLoading(page.allPropsFut) { spaceProps => 
    addExistingGadget <= new AddExistingPropertyGadget(page, thing, spaceProps, this)
  }
  val addExistingGadget = RxGadget[AddExistingPropertyGadget]
  
  lazy val createNew = AfterLoading(page.allTypesFut) { allTypes =>
    createNewGadget <= new CreateNewPropertyGadget(page, allTypes, this)
  }
  val createNewGadget = RxGadget[CreateNewPropertyGadget]
  
  def doRender() = {
    div(
      mainDiv <= (new WrapperDiv).initialContent(
        initButton <= new ButtonGadget(Info, icon("plus"), " Add a Property")({ () =>
          mainDiv.replaceContents(addExisting.rendered, true)
        })
      )
    )
  }
}
