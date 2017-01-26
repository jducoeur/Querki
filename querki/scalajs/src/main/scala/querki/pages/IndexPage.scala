package querki.pages

import scalatags.JsDom.all._
import upickle._
import autowire._

import querki.globals._
import querki.data.SpaceInfo
import querki.display.ButtonGadget
import querki.identity.UserLevel._
import querki.session.UserFunctions
import UserFunctions._

/**
 * @author jducoeur
 */
class IndexPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("index") {
  
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  def showSpace(space:SpaceInfo) = {
    li(IndexPage.spaceLink(space))
  }
    
  def spaceSection(title:String, section:Seq[SpaceInfo]) = {
    div(cls:="col-md-6",
      h3(title),
      ul(
        for {space <- section}
          yield showSpace(space)
      )
    )    
  }
      
  def awaitingValidationSection =
    div(cls:="jumbotron",
      h1("Welcome to Querki!"),
      p("""An email has been sent to your address, with a validation link. Please find that email and click on the
          |link in it -- that will finish activating your Querki account and let you start creating Spaces.""".stripMargin),
          
      p(new ButtonGadget(ButtonGadget.Normal, "Resend my activation email")({ () =>
        Client[UserFunctions].resendActivationEmail().call().foreach { _ =>
          StatusLine.showBriefly("Activation email sent!")
        }
      }))
    )
    
  def welcomeSection =
    div(cls:="jumbotron",
      h1("Welcome to Querki!"),
      p(s"""You're all set -- your Querki account is up and running. You can now create Spaces of your own by pressing
          |the ${msg("createButton")} button, below.""".stripMargin)
    )
          
  def pageContent = for {
    allSpaces <- Client[UserFunctions].listSpaces().call()
    awaitingValidation = (DataAccess.request.userLevel == PendingUser)
    noSpaces = allSpaces.mySpaces.isEmpty && allSpaces.memberOf.isEmpty
    guts =
      div(
        h1(pageTitle),
        
        if (awaitingValidation)
          awaitingValidationSection
        else if (noSpaces)
          // New user, who has no Spaces
          welcomeSection,
          
        if (!noSpaces)
          // Normal situation
          div(cls:="row",
            spaceSection(msg("ownSection"), allSpaces.mySpaces),
            spaceSection(msg("memberSection"), allSpaces.memberOf)
          ),
          
        // The CreateButton is disabled for unvalidated users. They can instantiate Apps, but that's it.
        p(new ButtonGadget(ButtonGadget.Normal, msg("createButton"), id:="_createSpaceButton", if (awaitingValidation) {disabled:=true})({ () =>
          Pages.createSpaceFactory.showPage()
        })),
        if (awaitingValidation) {
          p(cls:="_smallSubtitle", msg("notAllowedYet"))
        }
      )
  }
    yield PageContents(guts)
}

object IndexPage {
  def spaceLink(space:SpaceInfo) = {
    val spaceName = space.linkName.getOrElse(space.oid.underlying)
    a(href:=s"/u/${space.ownerHandle}/$spaceName/#!$spaceName", space.displayName)    
  }
  
  def spaceLink(space:SpaceInfo, display:String, page:PageFactory, params:(String, String)*) = {
    val pageUrl = page.pageUrl(params:_*)
    val spaceName = space.linkName.getOrElse(space.oid.underlying)
    a(href:=s"/u/${space.ownerHandle}/$spaceName/$pageUrl", display)    
  }
}
