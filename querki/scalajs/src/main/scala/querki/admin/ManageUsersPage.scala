package querki.admin

import scalatags.JsDom.all._
import autowire._

import querki.api.AdminFunctions
import querki.display.{ButtonGadget, ButtonKind}
import querki.globals._
import querki.pages._

class ManageUsersPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  def pageContent =
    for {
      pendingUsers <- Client[AdminFunctions].pendingUsers().call()
      guts =
        div(
          h1("Manage Users"),
          h3("Pending Users"),
          table(cls:="table table-hover",
            thead(
              td(b("Handle")), td(b("Email")), td(b())
            ),
            tbody(
	          for (user <- pendingUsers)
	            yield 
	              tr(td(user.mainHandle), td(user.email), 
	                td(new ButtonGadget(ButtonKind.Normal, "Upgrade")({ () =>
	                  Client[AdminFunctions].upgradePendingUser(user.userId).call().foreach { dummy =>
	                    PageManager.reload().flashing(false, s"Updated ${user.mainHandle} to full user")
	                  }
	                })))
            )
          )
        )
    }
      yield PageContents("Manage Users", guts)

}
