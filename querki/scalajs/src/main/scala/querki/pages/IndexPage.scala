package querki.pages

import scalatags.JsDom.all._
import upickle._
import autowire._

import querki.globals._
import querki.data.SpaceInfo
import querki.session.UserFunctions
import UserFunctions._

/**
 * @author jducoeur
 */
class IndexPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  
  def showSpace(space:SpaceInfo) = {
    val spaceName = space.linkName.getOrElse(space.oid.underlying)
    li(a(href:=s"/u/${space.ownerHandle}/$spaceName/#$spaceName", space.displayName))
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
    guts =
      div(
        h1("Your Spaces"),
        div(cls:="row",
          spaceSection("Spaces You Own", allSpaces.mySpaces),
          spaceSection("Spaces You are a Member of", allSpaces.memberOf)
        )
      )
  }
    yield PageContents("Your Spaces", guts)
}
