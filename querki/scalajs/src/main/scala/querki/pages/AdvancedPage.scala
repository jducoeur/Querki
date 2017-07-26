package querki.pages

import scala.util.{Failure, Success}

import org.scalajs.dom

import scalatags.JsDom.all._
import autowire._

import querki.api._
import querki.data.SpaceInfo
import querki.display.{ButtonGadget, Dialog, QText}
import querki.display.rx.QGadgetRef
import querki.globals._
import querki.security.SecurityFunctions

class AdvancedPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("advancedCommands") {
  lazy val thingId = TID(params("thingId"))
  
  lazy val Apps = interface[querki.apps.Apps]
  lazy val Client = interface[querki.client.Client]
  lazy val History = interface[querki.history.History]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  def archiveAfterConfirm(info:SpaceInfo) = {
    val archiveDialog:Dialog = 
      new Dialog("Confirm Archive",
        p(b(s"Are you sure you want to archive the entire Space ${info.displayName}? It may be a number of months before you can retrieve it!")),
        (ButtonGadget.Danger, Seq("Archive", id := "_archiveConfirm"), { dialog => 
          // TODO: display a spinner
          Client[SecurityFunctions].archiveThisSpace().call().onComplete { 
            case Success(b) if (b == true) => {
              // This Space has been archived, so we're no longer allowed to call it. Navigate to index, since that's
              // pretty much the only thing we can do:
              PageManager.showIndexPage()
            }
            case Failure(ex) => {
              ex match {
                case CanNotArchiveException() => StatusLine.showUntilChange("You are not allowed to archive this Space!")
                case _ => StatusLine.showUntilChange(ex.toString())
              }
            }
          }
        }),
        (ButtonGadget.Normal, Seq("Cancel", id := "_archiveCancel"), { dialog => dialog.done() })
      )
    archiveDialog.show()
  }
  
  val ql =
    s"""""### Advanced commands for ____
        |
        |Currently running on *[[_clusterAddress]]*
        |
        |**[Export all Instances of [[Link Name]] as a CSV file](_exportModel?modelId=[[_oid]]&format=1)**
        |
        |[[_orphanedInstances -> _section(""### Orphaned Instances"", 
        |""These Instances belong to missing Models; we recommend going into each one, opening Advanced Edit from the
        |Actions menu, and changing their Model.
        |
        |[[_bulleted]]
        |
        |------"")]]
        |
        |[[_if(_hasPermission(Who Can Read Comments._self), 
        |  ""**Send me a Message whenever someone comments in this Space:** [[_space -> _getCommentNotifications._edit]]
        |("Maybe" means the default: Yes if you are the owner of this space, No otherwise.)
        |**Note:** this may not take effect for a few hours."")]]""""".stripMargin
        
  val exportedXML = QGadgetRef.of[dom.html.Div]

  def pageContent = for {
    gutsRaw <- Client[ThingFunctions].evaluateQL(thingId, ql).call()
    guts = 
      div(
        new QText(gutsRaw),
        if (DataAccess.request.isOwner) {
          div(
            p("""Press this button to reload this Space. (This is mainly for testing; you can usually ignore it.)"""),
            new ButtonGadget(ButtonGadget.Info, "Reload", id := "_reloadButton") ({ () =>
              Client[ThingFunctions].reloadSpace().call().foreach { _ =>
                PageManager.reload().map { _ =>
                  StatusLine.showUntilChange("Reloaded")
                }
              }
            }),
            
            p("""Press this button to view the history of this Space"""),
            new ButtonGadget(ButtonGadget.Info, "View History", id := "_historyButton") ({ () =>
              History.historySummaryFactory.showPage()
            }),
              
            p("""Press this button to export this *entire* Space as XML. (NOTE: photographs are not exported.)
              The exported XML will open in a new tab or window, which you can save if you want."""),
            a(
              cls:="btn btn-default",
              target:="_blank",
              href:="_export.xml",
              "Export Space to XML"
            ),
            
            p("""Press this button to Archive this Space. NOTE: there is currently no way to list or recover Archived
              Spaces! Those capabilities are at least a few months away, so only Archive this Space if you are sure
              you aren't going to need it any time soon!"""),
            new ButtonGadget(ButtonGadget.Danger, "Archive this Space") ({ () =>
              archiveAfterConfirm(DataAccess.space.get)
            })
          )
        },
        new ButtonGadget(ButtonGadget.Primary, "Done")({ () => 
          Pages.thingPageFactory.showPage(thingId)
        })
      )
  }
    yield PageContents(pageTitle, guts)
}
