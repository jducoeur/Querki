package querki.pages

import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.SecurityFunctions

class SharingPage(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  lazy val space = DataAccess.space.get

  def pageContent = for {
    roles <- Client[SecurityFunctions].getRoles().call()
    guts =
      div(
        p("The Roles are:"),
        ul(
          roles.map(role => li(role.displayName))
        )
      )
  }
    yield PageContents(s"Manage Sharing for ${space.displayName}", guts)
}
