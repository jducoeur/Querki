package querki.datamodel

import scalatags.JsDom.all._

import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.Dialog

class DataModelEcot(e:Ecology) extends ClientEcot(e) with DataModel {
  
  def implements = Set(classOf[DataModel])
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Pages = interface[querki.pages.Pages]
  
  def deleteAfterConfirm(thing:ThingInfo) = {
    val deleteDialog:Dialog = 
      new Dialog("Confirm Delete", 200, 350,
        p(b(s"Are you sure you want to delete ${thing.displayName}? There is currently no way to get it back!")),
        ("Delete" -> { dialog => 
          println(s"I'm about to delete ${thing.displayName}");
          // TODO: display a spinner
          Client[ThingFunctions].deleteThing(thing.oid).call().foreach { dummy =>
            dialog.done()
            Pages.flashMessage(false, s"${thing.displayName} deleted.")
            Pages.showSpacePage(DataAccess.space.get)
          }
        }),
        ("Cancel" -> { dialog => dialog.done() })
      )
    deleteDialog.show()
  }
}
