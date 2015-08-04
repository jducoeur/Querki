package querki.pages

import scala.concurrent.Future

import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.jquery._

import querki.display.ButtonGadget
import querki.display.rx._
import querki.globals._
import querki.session.UserFunctions

/**
 * @author jducoeur
 */
class CreateSpacePage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  lazy val Client = interface[querki.client.Client]
  
  val spaceName = GadgetRef[RxInput]
  
  def nameFilter(evt:JQueryEventObject):Boolean = {
    // TODO: this is quite crude, and doesn't allow Unicode through. We should do better.
    // See if there is a library to really do this sort of keyboard filtering well.
    val key = evt.which
    val c = key.toChar
    
    (c >= 'A' && c <= 'Z') || 
    // Numeric keypad
    (key >= 96 && key <= 105) ||
    // Backspace and Tab and Enter
    key == 8 || key == 9 || key == 13 ||
    // Home and End
    key == 35 || key == 36 ||
    // left and right arrows
    key == 37 || key == 39 ||
    // Del and Ins
    key == 46 || key == 45 ||
    (!(evt.shiftKey.get) && 
      (c >= '0' && c <= '9') || 
      key == 189 || // dash 
      c == ' ')    
  }
  
  // Weird -- I think it's a page that we can create without going to the server!
  def pageContent = {
    val guts =
      div(
        h1("Create a New Space"),
        form(
          div(cls:="form-group col-md-12",
            div(cls:="input-group",
              spaceName <= new RxInput(Some(nameFilter _), "text", cls:="form-control", maxlength:=254, placeholder:="New Space Name", tabindex:=200),
              span(cls:="input-group-btn",
                new ButtonGadget(ButtonGadget.Normal, "Create Space", tabindex:=210, 
                    disabled := Rx { spaceName.isEmpty || spaceName.get.text().length() == 0 })
                ({ () =>
                  val newName = spaceName.get.text()
                  Client[UserFunctions].createSpace(newName).call() foreach { space =>
                    val spaceName = space.linkName.getOrElse(space.oid.underlying)
                    val url = s"/u/${space.ownerHandle}/$spaceName/#$spaceName"
                    PageManager.navigateTo(url)
                  }
                })
              )
            )
          )
        ),
        hr(),
        h3("Or..."),
        p(
          new ButtonGadget(ButtonGadget.Normal, "Import a Space from a file", tabindex:=300)({ () =>
            Pages.importSpaceFactory.showPage()
          })
        )
      )
      
    Future.successful(PageContents("Create a New Space", guts))
  }
}