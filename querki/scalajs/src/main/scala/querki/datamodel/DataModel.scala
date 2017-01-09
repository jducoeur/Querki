package querki.datamodel

import scala.util.Success

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import autowire._

import models.Kind

import querki.globals._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.{ButtonGadget, Dialog}
import querki.display.rx.RxSelect
import querki.editing.EditFunctions
import EditFunctions.ChangePropertyValue
import querki.pages.PageImplicits

class DataModelEcot(e:Ecology) extends ClientEcot(e) with DataModel with querki.util.ScalatagUtils with PageImplicits {
  
  def implements = Set(classOf[DataModel])
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val ProgressDialog = interface[querki.display.ProgressDialog]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  def showSpaceAfterDelete(thing:ThingInfo, recoverMsg:Option[String] = None) = {
    Pages.showSpacePage(DataAccess.space.get).
      flashing(false, 
        s"${thing.displayName} deleted. ",
        b(a(href:=Pages.undeleteFactory.pageUrl("thingId" -> thing.oid.underlying, "thingName" -> thing.displayName), "Click here to undelete it.")),
        s" ${recoverMsg.getOrElse("")}")
  }
  
  def doDelete(thing:ThingInfo, dialogOpt:Option[Dialog] = None, recoverMsg:Option[String] = None) = {
    Client[ThingFunctions].deleteThing(thing.oid).call().foreach { dummy =>
      dialogOpt.map(_.done())
      showSpaceAfterDelete(thing, recoverMsg)
    }
  }
  
  def deleteAfterConfirm(thing:ThingInfo) = {
    if (thing.kind == Kind.Property) {
      Client[EditFunctions].getPropertyUsage(thing.oid).call() foreach { usage =>
        // We require pretty dire confirmation if the Property is used anywhere.
        if (usage.nModels > 0 || usage.nInstances > 0) {
          val deleteDialog:Dialog = 
            new Dialog("Confirm Delete",
              p(b(s"""Property ${thing.displayName} is currently being used by ${usage.nModels} Models and 
                      |${usage.nInstances} Instances. If you delete it, it will be removed from all of those,
                      |and the values of ${thing.displayName} will be dropped. You can lose data this way!""".stripMargin)),
              (ButtonGadget.Warning, Seq(s"Remove all values, then delete ${thing.displayName}", id := "_removeAllConfirm"), { dialog => 
                println(s"I'm about to delete ${thing.displayName}");
                // TODO: display a spinner
                Client[EditFunctions].removePropertyFromAll(thing.oid).call().foreach { handle =>
                  dialog.done()
                  ProgressDialog.showDialog(
                    s"deleting ${thing.displayName}", 
                    handle, 
                    showSpaceAfterDelete(thing),
                    StatusLine.showBriefly(s"Error while deleting ${thing.displayName}"))
                }
              }),
              (ButtonGadget.Normal, Seq("Cancel", id := "_cancelDelete"), { dialog => dialog.done() })
            )
          deleteDialog.show()
        } else {
          // The Property isn't in use, so just let it go:
          doDelete(thing)      
        }
      }
    } else {
      if (thing.isModel) {
        Client[ThingFunctions].getNumInstances(thing).call() map { nInstances =>
          if (nInstances == 0)
            // It's an unused Model, so safe to delete:
            doDelete(thing)
          else {
            val deleteDialog:Dialog = 
              new Dialog("Confirm Delete",
                p(b(raw(s"""There are currently $nInstances instances of ${thing.displayName}. If you delete it, they will be
                |left orphaned, and may no longer work right! You will not be able to undo this! Are you sure you
                |want to delete ${thing.displayName}?""".stripMargin))),
                (ButtonGadget.Warning, Seq("Delete", id := "_confirmDelete"), { dialog => 
                  doDelete(thing, Some(dialog), Some("You can find the ophaned Instances by going to Actions -> Advanced..."))
                }),
                (ButtonGadget.Normal, Seq("Cancel", id := "_cancelDelete"), { dialog => dialog.done() })
              )
            deleteDialog.show()
          }
        }
      } else {
        // It's just an Instance:
        doDelete(thing)
      }
    }
  }
  
  private def modelSelectionForm(formTitle:String, prompt:String, selectButton:String, onSelect:TID => Unit, onCancel: => Unit) = 
  {
    for {
      typeInfo <- DataAccess.getAllTypes()
      stdThings <- DataAccess.standardThings
    }
    {
      val modelOptions = Var({
        val modelOpts = 
          typeInfo.
          models.
          filter(_.isInstantiatable).
          sortBy(_.displayName).
          map(model => option(value:=model, model.displayName))
        option(value:=stdThings.basic.simpleThing, "Simple Thing") +: modelOpts
      })
      val selector = RxSelect(modelOptions, id:="_modelSelector")
    
      val modelDialog:Dialog = 
        new Dialog(formTitle,
          div(
            p(prompt),
            selector
          ),
          (ButtonGadget.Primary, Seq(selectButton, id := "_modelSelected"), { dialog =>
            onSelect(selector.selectedTID())
            dialog.done()
          }),
          (ButtonGadget.Normal, Seq("Cancel", id := "_modelCancel"), { dialog => 
            onCancel
            dialog.done() 
          })
        )
    
      modelDialog.show()
    }
  }
  
  def chooseAModel(title:String, msg:String, buttonText:String):Future[Option[TID]] = {
    val promise = Promise[Option[TID]]
    modelSelectionForm(
      title, 
      msg,
      buttonText,
      { selection => promise.complete(Success(Some(selection))) },
      {
        promise.complete(Success(None))
        PageManager.showRoot()
      }
    )
    promise.future
  }
  
  def changeModel(thing:ThingInfo, cb:ThingInfo => Unit) = {
    modelSelectionForm(
      s"Change Model for ${thing.displayName}",
      s"Choose the Model that ${thing.displayName} should now be based on:",
      "Change Model",
      { selection =>
        Client[EditFunctions].changeModel(thing, selection).call().foreach { newThingInfo =>
          cb(newThingInfo)
        }
      },
      {}
    )
  }
}
