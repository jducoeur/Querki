package querki.security

import scala.concurrent.Future
import scalatags.JsDom.all._
import autowire._
import rx._
import org.querki.gadgets._
import querki.display.{TabGadget, ButtonGadget, Dialog}
import ButtonGadget._
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
  lazy val Client = page.interface[querki.client.Client]

  val personDisplays: Var[Seq[PersonDisplay]] = Var(Seq.empty)
  val selectedPersons: Rx[Seq[PersonDisplay]] = Rx { personDisplays().filter(_.selected()) }
  val personsAreSelected: Rx[Boolean] = Rx { !selectedPersons().isEmpty }

  val sortedMembers: Seq[PersonInfo] = members.sortBy(_.person.displayName.toLowerCase)

  lazy val deleteButton =
    new ButtonGadget(Info,
      disabled := Rx { !personsAreSelected() },
      "Remove Selected Members",
      id := "_removeSelectedMembersButton"
    )({ () =>
      deleteDialog.show()
    })

  lazy val deleteDialog: Dialog =
    new Dialog(
      "Confirm Remove Members",
      p(s"You are about to remove the selected Members from this Space. Are you sure?"),
      (ButtonGadget.Warning, Seq("Remove Members", id := "_confirmRemoveButton"), { dialog =>
        dialog.done()
        val tidsToRemove = selectedPersons.now.map(_.person.person.oid)
        Client[SecurityFunctions].removeFromSpace(tidsToRemove).call().map { _ =>
          page.PageManager.reload()
        }
      }),
      (ButtonGadget.Normal, Seq("Cancel", id := "_cancelDelete"), { dialog => dialog.done() })
    )

  def tabContent =
    for {
      dummy <- Future.successful(())
      guts =
        div(
          h3("Members"),
          p("The following people are members of this Space. Click on a member's Role in order to change it. Click on a row to select that member."),

          p("To remove members from this Space, select them by clicking on their rows, and then press this button:"),
          p(deleteButton),
          
          table(cls:="table table-hover",
            tbody(
              for {
                member <- sortedMembers
                display = new PersonDisplay("info", member, roleMap, customMap, page.std, page)
                _ = personDisplays() = { personDisplays.now :+ display }
              }
                yield display
            )
          )
        )
    }
      yield guts
}
