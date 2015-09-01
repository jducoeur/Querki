package querki.datamodel

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import autowire._

import models.Kind

import querki.globals._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.Dialog
import querki.display.rx.RxSelect
import querki.editing.EditFunctions
import EditFunctions.ChangePropertyValue
import querki.pages.PageImplicits

class DataModelEcot(e:Ecology) extends ClientEcot(e) with DataModel with querki.util.ScalatagUtils with PageImplicits {
  
  def implements = Set(classOf[DataModel])
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val ProgressDialog = interface[querki.display.ProgressDialog]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  def deleteAfterConfirm(thing:ThingInfo) = {
    if (thing.kind == Kind.Property) {
      Client[EditFunctions].getPropertyUsage(thing.oid).call() foreach { usage =>
        // We require pretty dire confirmation if the Property is used anywhere.
        // TODO: we might soften this for Models, but for now we're going to be hardcore.
        if (usage.nModels > 0 || usage.nInstances > 0) {
          val deleteDialog:Dialog = 
            new Dialog("Confirm Delete", 350, 350,
              p(b(s"""Property ${thing.displayName} is currently being used by ${usage.nModels} Models and 
                      |${usage.nInstances} Instances. If you delete it, it will be removed from all of those,
                      |and the values of ${thing.displayName} will be dropped. You can lose data this way!""".stripMargin)),
              (s"Remove all values, then delete ${thing.displayName}" -> { dialog => 
                println(s"I'm about to delete ${thing.displayName}");
                // TODO: display a spinner
                Client[EditFunctions].removePropertyFromAll(thing.oid).call().foreach { handle =>
                  dialog.done()
                  ProgressDialog.showDialog(
                    s"deleting ${thing.displayName}", 
                    handle, 
                    Pages.showSpacePage(DataAccess.space.get).flashing(false, s"${thing.displayName} deleted."), 
                    StatusLine.showBriefly(s"Error while deleting ${thing.displayName}"))
                }
              }),
              ("Cancel" -> { dialog => dialog.done() })
            )
          deleteDialog.show()
        } else {
          // The Property isn't in use, so just let it go:
          Client[ThingFunctions].deleteThing(thing.oid).call().foreach { dummy =>
            Pages.showSpacePage(DataAccess.space.get).flashing(false, s"${thing.displayName} deleted.")
          }          
        }
      }
    } else {
      val deleteDialog:Dialog = 
        new Dialog("Confirm Delete", 200, 350,
          p(b(s"Are you sure you want to delete ${thing.displayName}? There is currently no way to get it back!")),
          ("Delete" -> { dialog => 
            println(s"I'm about to delete ${thing.displayName}");
            // TODO: display a spinner
            Client[ThingFunctions].deleteThing(thing.oid).call().foreach { dummy =>
              dialog.done()
              Pages.showSpacePage(DataAccess.space.get).flashing(false, s"${thing.displayName} deleted.")
            }
          }),
          ("Cancel" -> { dialog => dialog.done() })
        )
      deleteDialog.show()
    }
  }
  
  private def modelSelectionForm(formTitle:String, prompt:String, selectButton:String, onSelect:TID => Unit) {
    for {
      typeInfo <- DataAccess.getAllTypes()
      stdThings <- DataAccess.standardThings
    }
    {
      val modelOptions = Var({
        val modelOpts = typeInfo.models.sortBy(_.displayName).map(model => option(value:=model, model.displayName))
        option(value:=stdThings.basic.simpleThing, "Simple Thing") +: modelOpts
      })
      val selector = RxSelect(modelOptions)
    
      val modelDialog:Dialog = 
        new Dialog(formTitle, 300, 350,
          div(
            p(prompt),
            selector
          ),
          (selectButton -> { dialog =>
            onSelect(selector.selectedTID())
            dialog.done()
          }),
          ("Cancel" -> { dialog => dialog.done() })
        )
    
      modelDialog.show()
    }
  }
  
  def designAModel() = {
    modelSelectionForm(
      "Design a Model", 
      "Choose which Model to base the new one on (just use Simple Thing if not sure):",
      "Create Model",
      { selection =>
        DataAccess.standardThings.foreach { stdThings =>
          val initProps = 
            Seq(
              ChangePropertyValue(Editing.propPath(stdThings.core.isModelProp), Seq("true")),
              // The Display Name is usually an Instance Property:
              ChangePropertyValue(Editing.propPath(stdThings.editing.instancePropsProp), Seq(stdThings.basic.displayNameProp.oid.underlying))
            )
          Client[EditFunctions].create(selection, initProps).call().foreach { modelInfo =>
            Editing.modelDesignerFactory.showPage(modelInfo)
          }
        }
      })
  }
  
  def createAThing() = {
    modelSelectionForm(
      "Create a Thing",
      "What kind of Thing do you want to create? (Just use Simple Thing if you just want a plain page.)",
      "Create",
      { selection => Pages.createAndEditFactory.showPage(selection) }
    )
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
      }
    )
  }
}
