package querki.admin

import scalatags.JsDom.all._
import autowire._

import querki.api._
import querki.globals._
import querki.pages._

class StatisticsPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  
  def pageContent =
    for {
      stats <- Client[AdminFunctions].statistics().call()
      guts =
        div(
          h1("Current Querki Statistics"),
          p(s"Invitees: ${stats.nInvitees} Full Users: ${stats.nFullUsers} Test Users: ${stats.nTestUsers} Spaces: ${stats.nSpaces}")
        )
    }
      yield PageContents("Current Querki Statistics", guts)
}
