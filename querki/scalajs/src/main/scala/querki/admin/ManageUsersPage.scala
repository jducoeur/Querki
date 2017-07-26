package querki.admin

import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._
import rx._
import org.querki.gadgets._
import org.querki.jquery._

import querki.api._
import AdminFunctions._
import querki.display.{ButtonGadget}
import querki.display.rx._
import querki.globals._
import querki.identity.UserLevel._
import querki.pages._

class ManageUsersPage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {
  
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  implicit class CallExt[T](fut:Future[T]) {
    def checkForeach[U](cb:(T) => U) = {
      fut.onFailure {
        case NotAnAdminException() => Pages.showSpacePage(DataAccess.space.get).flashing(true, "Manage Users is for Querki Administrators only. Sorry.")
      }
      fut.foreach(cb)
    }
  }
  
  // TODO: add the ability for Superadmin to create Admins
  lazy val levels = Seq(
    PendingUser,
    FreeUser,
    PaidUser,
    PermanentUser,
    TestUser
  ) ++ (if (myLevel == SuperadminUser) Seq(AdminUser) else Seq.empty)
  
  lazy val myLevel = DataAccess.request.userLevel
    
  class UserView(user:AdminUserView) extends Gadget[dom.html.TableRow] {
    lazy val levelOptions = 
      Var(levels.map { level =>
        option(
          value:=level,
          if (level == user.level) selected:="selected",
          levelName(level).capitalize)
      })
      
    val levelSelector = GadgetRef[RxSelect].whenSet { g => 
      Obs(g.selectedOption, skipInitial=true) {
        g.selectedOption().map { opt =>
          val newLevel = Integer.parseInt(opt.valueString)
          if (newLevel != user.level) {
            Client[AdminFunctions].changeUserLevel(user.userId, newLevel).call().checkForeach { newView =>
              $(elem).replaceWith(new UserView(newView).render)
              StatusLine.showBriefly(s"Switched ${newView.mainHandle} to ${levelName(newView.level)}")
            }
          }
        }
      }
    }
    
    def colorClass = {
      user.level match {
        case PendingUser => "warning"
          
        case FreeUser | PaidUser | PermanentUser => ""
          
        case TestUser => "active"
          
        case AdminUser | SuperadminUser => "success"
          
        case _ => "danger"
      }
    }
    
    def doRender() = {
      tr(cls:=colorClass,
        td(user.mainHandle), 
        td(user.email), 
        if (user.level == SuperadminUser)
          td("Superadmin")
        else if (user.level == AdminUser && myLevel != SuperadminUser)
          td("Admin")
        else
          td(levelSelector <= RxSelect(levelOptions))
      )
    }
  }
  
  lazy val allUsersButton =
    new RunButton(ButtonGadget.Normal, "Fetch all users", "Fetching...")({ btn =>
      Client[AdminFunctions].allUsers().call.checkForeach { userList =>
        allUsersSection <=
          div(
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
  val allUsersSection = GadgetRef.of[dom.html.Div]
  
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
                tr(cls:="warning", td(user.mainHandle), td(user.email), 
                  td(new RunButton(ButtonGadget.Normal, "Upgrade", "Upgrading...")({ btn =>
                    Client[AdminFunctions].upgradePendingUser(user.userId).call().checkForeach { upgraded =>
                      PageManager.reload().flashing(false, s"Updated ${user.mainHandle} to full user")
                    }
                  })))
            )
          ),
          hr,
          allUsersButton,
          allUsersSection <= div(display:="none")
        )
    }
      yield PageContents("Manage Users", guts)

}
