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
class IndexPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  
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
  
  def pageContent = for {
    allSpaces <- Client[UserFunctions].listSpaces().call()
    canCreate = (DataAccess.request.userLevel >= PendingUser)
    guts =
      div(
        h1("Your Spaces"),
        div(cls:="row",
          spaceSection("Spaces You Own", allSpaces.mySpaces),
          spaceSection("Spaces You are a Member of", allSpaces.memberOf)
        ),
        p(new ButtonGadget(ButtonGadget.Normal, "Create a new Space", id:="_createSpaceButton", if (!canCreate) {disabled:=true})({ () =>
          Pages.createSpaceFactory.showPage()
        })),
        if (!canCreate) {
          p(cls:="_smallSubtitle", 
            "As a new user, you are not yet allowed to create Spaces. Check back soon - you should be upgraded within a few days.")
        }
      )
  }
    yield PageContents("Your Spaces", guts)
}

object IndexPage {
  def spaceLink(space:SpaceInfo) = {
    val spaceName = space.linkName.getOrElse(space.oid.underlying)
    a(href:=s"/u/${space.ownerHandle}/$spaceName/#!$spaceName", space.displayName)    
  }
}