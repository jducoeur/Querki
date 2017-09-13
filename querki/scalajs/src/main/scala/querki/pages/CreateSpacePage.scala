package querki.pages

import scala.concurrent.Future

import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._
import rx._
import org.querki.gadgets._

import org.querki.jquery._

import querki.data.SpaceInfo
import querki.display.ButtonGadget
import querki.display.rx._
import querki.globals._
import querki.session.UserFunctions
import querki.util.InputUtils

/**
 * @author jducoeur
 */
class CreateSpacePage(params:ParamMap)(implicit val ecology:Ecology) extends Page("createSpace") {
  lazy val Client = interface[querki.client.Client]
  
  val spaceName = GadgetRef[RxInput]
    .whenSet { g => 
      g.onEnter { text =>
        if (text.length() > 0) {
          createSpace()
        }
      }
    }
  
  def createSpace():Unit = {
    val newName = spaceName.get.text.now.trim
    Client[UserFunctions].createSpace(newName, None).call() foreach { space =>
      CreateSpacePage.navigateToSpace(space)
    }    
  }
    
  // Weird -- I think it's a page that we can create without going to the server!
  def pageContent = {
    val guts =
      div(
        h1(pageTitle),
        div(cls:="col-md-12",
          form(
            p("""To create a Space, type its name (letters, numbers and spaces only) below, and press the button."""),
            div(cls:="form-group col-md-6",
              spaceName <= new RxInput(
                  Some(InputUtils.spaceNameFilter _), "text", 
                  id:="_newSpaceName", cls:="form-control", maxlength:=254, placeholder:=msg("namePlaceholder"), tabindex:=200),
              span(cls:="input-group-btn",
                new ButtonGadget(ButtonGadget.Normal, msg("createButton"), tabindex:=210, id:="_createSpaceButton", 
                    disabled := Rx { spaceName.isEmpty || spaceName.get.text().length() == 0 })
                ({ () => createSpace()  })
              )
            )
          )
        ),
        hr(),
        h3("Or..."),
        p(b("Advanced: "), 
          """If you have a Space that was exported from Querki in XML format, or you have a MySQL database to import
            |as a Querki Space, then press this button instead.""".stripMargin),
        p(
          new ButtonGadget(ButtonGadget.Normal, msg("importButton"), id:="_importButton", tabindex:=300)({ () =>
            Pages.importSpaceFactory.showPage()
          })
        )
      )
      
    Future.successful(PageContents(guts))
  }
}

object CreateSpacePage {
  def navigateToSpace(space:SpaceInfo)(implicit ecology:Ecology) = {
    val PageManager = ecology.api[querki.display.PageManager]
    val Editing = ecology.api[querki.editing.Editing]
    
    val spaceName = space.linkName.getOrElse(space.oid.underlying)
    // After creating a new Space, we start the user out in Edit Space Info.
    // TODO: this hardcoded URL bites. How can we make this suck less?
    val url = s"/u/${space.ownerHandle}/$spaceName/${Editing.editSpaceInfoFactory.pageUrl()}"
    PageManager.navigateTo(url)    
  }
}
