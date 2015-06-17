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
import RxGadget._

class AddPropertyGadget(page:ModelDesignerPage, thing:ThingInfo)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val mainDiv = RxGadget[WrapperDiv]
  
  lazy val initButton:ButtonGadget = new ButtonGadget(Info, icon("plus"), " Add a Property")({ () =>
    mainDiv.replaceContents(addExisting.rendered, true)
  })
  
  lazy val cancelButton = new ButtonGadget(Normal, "Cancel")({ () => reset() })
  
  val stdThingFut = DataAccess.standardThings
  def allTypesFut = page.allTypesFut
  def allPropsFut = page.allPropsFut
  
  def reset() = {
    addExistingGadget().map(_.reset())
    createNewGadget().map(_.reset())
    mainDiv.replaceContents(initButton.rendered, true)
  }
  
  lazy val addExisting = AfterLoading(allPropsFut) { spaceProps => 
    val g = new AddExistingPropertyGadget(page, thing, spaceProps, this)
    addExistingGadget() = Some(g)
    g
  }
  // This is a bit boilerplatey, but we're trying not to evaluate addExisting unnecessarily
  // TODO: should we enhance AfterLoading to be able to put the laziness into there
  // explicitly?
  val addExistingGadget = Var[Option[AddExistingPropertyGadget]](None)
  
  lazy val createNew = AfterLoading(allTypesFut) { allTypes =>
    val g = new CreateNewPropertyGadget(page, allTypes, this)
    createNewGadget() = Some(g)
    g
  }
  val createNewGadget = Var[Option[CreateNewPropertyGadget]](None)
  
  def doRender() = {
    div(
      mainDiv((new WrapperDiv).initialContent(initButton))
    )
  }
}
