package querki.security

import scala.concurrent.Future

import scalatags.JsDom.all._
import rx._

import querki.display.TabGadget
import querki.globals._
import querki.pages.Page

class MembersTab(
    roleMap: RoleInfo,
    customMap: RoleInfo,
    members: Seq[PersonInfo],
    page: Page
  )(implicit val ecology: Ecology, ctx: Ctx.Owner)
  extends TabGadget(SharingPage.Tab.Members.entryName, "members", "Members")
{
  def tabContent =
    for {
      dummy <- Future.successful(())
      guts =
        div(
          h3("Members"),
          p("The following people are members of this Space. Click on a member's Role in order to change it. Click on a row to select that member."),
          
          table(cls:="table table-hover",
            tbody(
              for (member <- members) 
                yield new PersonDisplay("info", member, roleMap, customMap, page.std)
            )
          )
        )
    }
      yield guts
}
