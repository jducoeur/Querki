package querki.admin

import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._
import rx._

import querki.api.AdminFunctions
import AdminFunctions._
import querki.display.{ButtonKind, Gadget}
import querki.display.rx.{RxDiv, RxButton, RxSelect}
import querki.globals._
import querki.identity.UserLevel._
import querki.pages._

class ManageUsersPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  // TODO: add the ability for Superadmin to create Admins
  lazy val levels = Seq(
    PendingUser,
    FreeUser,
    PaidUser,
    PermanentUser,
    AdminUser,
    SuperadminUser
  )
    
  class UserView(user:AdminUserView) extends Gadget[dom.html.TableRow] {
    lazy val levelOptions = 
      Var(levels.map { level =>
        option(
          value:=level,
          if (level == user.level) selected:="selected",
          levelName(level).capitalize)
      })
    lazy val levelSelector = RxSelect(levelOptions)
    
    lazy val choiceObserver = Obs(levelSelector.selectedOption, skipInitial=true) {
      levelSelector.selectedOption().map { opt =>
        val newLevel = Integer.parseInt(opt.valueString)
        Client[AdminFunctions].changeUserLevel(user.userId, newLevel).call().foreach { newView =>
          // TODO: replace the value in-place:
          PageManager.reload().flashing(false, s"Changed ${user.mainHandle} to ${levelName(newLevel)}")
        }
      }
    }
    
    override def onCreate(e:dom.html.TableRow) = {
      // This needs to be kicked to life *after* rendering:
      choiceObserver
    }
    
    def doRender() = {
      tr(
        td(user.mainHandle), 
        td(user.email), 
        td(levelSelector)
      )
    }
  }
  
  lazy val allUsersButton =
    new RxButton(ButtonKind.Normal, "Fetch all users", "Fetching...")({ btn =>
      Client[AdminFunctions].allUsers().call.foreach { userList =>
        allUsers() =
          Seq[Gadget[_]](
            h3("All Users"),
            table(cls:="table table-hover",
              thead(
                td(b("Handle")), td(b("Email")), td(b())
              ),
              tbody(
	            for (user <- userList)
  	              yield new UserView(user)
              )
            )
          )
        btn.done()
      }
    })
  lazy val allUsers = Var[Seq[Gadget[_]]](Seq.empty)
  lazy val allUsersSection = RxDiv(allUsers)
  
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
	                td(new RxButton(ButtonKind.Normal, "Upgrade", "Upgrading...")({ btn =>
	                  Client[AdminFunctions].upgradePendingUser(user.userId).call().foreach { upgraded =>
	                    PageManager.reload().flashing(false, s"Updated ${user.mainHandle} to full user")
	                  }
	                })))
            )
          ),
          hr,
          allUsersButton,
          allUsersSection
        )
    }
      yield PageContents("Manage Users", guts)

}
