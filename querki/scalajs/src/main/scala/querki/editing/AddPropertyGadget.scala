package querki.editing

import org.scalajs.dom.{raw => dom}
import rx._
import scalatags.JsDom.all._
import org.querki.gadgets._

import querki.globals._

import querki.data.{TID => _TID, _}
import querki.display.{ButtonGadget, QuerkiUIUtils}
import ButtonGadget._

class AddPropertyGadget(page:ModelDesignerPage, thing:ThingInfo)
    (implicit val ecology:Ecology, ctx:Ctx.Owner)
  extends Gadget[dom.HTMLDivElement] with QuerkiUIUtils with EcologyMember 
{
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val theGadget = GadgetRef[Gadget[_]]
  lazy val cancelButton = new ButtonGadget(Normal, page.msg("addPropertyCancel"))({ () => reset() })
  
  def makeInitButton() = 
    new ButtonGadget(Info, icon("plus"), id:="_addPropertyButton", page.msg("addPropertyButton"))({ () =>
      showAddExisting()
    })
    
  def showAddExisting() = {
    page.allPropsFut.foreach { spaceProps =>
      theGadget <= new AddExistingPropertyGadget(page, thing, spaceProps, this)
    }    
  }
  
  def showCreateNew() = {
    page.allTypesFut.foreach { allTypes =>
      theGadget <= new CreateNewPropertyGadget(page, allTypes, this)
    }
  }
  
  def reset() = {
    theGadget <= makeInitButton()
  }
  
  def doRender() = {
    div(
      theGadget <= makeInitButton()
    )
  }
}
