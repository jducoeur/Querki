package querki.publication

import scalatags.JsDom.all._
import autowire._
import rx._

import querki.ecology._
import querki.editing.EditFunctions
import EditFunctions.ChangePropertyValue
import querki.globals._
import querki.pages._
import querki.display.input.InputGadget
import querki.display.rx._
import querki.security.{LevelMap, OnePerm, SecurityFunctions}
import SecurityFunctions._

class EditPublicationPage(params:ParamMap)(implicit val ecology:Ecology) 
  extends Page("editPublish") with LevelMap
{
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]
  
  val modelId = TID(params.requiredParam("modelId"))
  
  lazy val readPermId = std.security.canReadPerm.oid
  
  def pageContent = {
    for {
      model <- DataAccess.getThing(modelId)
      // TODO: provide some more-useful feedback if this sanity-check ever fails:
      if (model.isModel)
      allPerms <- Client[SecurityFunctions].getAllPerms().call()
      readPermInfo = allPerms.find(_.id == readPermId).get
      afterPerm <- Client[SecurityFunctions].getOnePerm(std.publication.canReadAfterPublishPerm).call()
      thingPerms <- Client[SecurityFunctions].permsFor(model).call().map(Var(_))
      instanceThing = thingPerms().instancePermThing.getOrElse(model)
      instanceReadVar = Rx { thingPerms().instancePerms.find(_.permId == readPermId) }
      publishable = Var[Boolean](model.hasFlag(std.publication.publishableProp))
      watcher = Obs(publishable, skipInitial=true) {
        val path = Editing.propPath(std.publication.publishableProp, Some(model.oid))
        val msg = ChangePropertyValue(path, List(publishable().toString))
        InputGadget.doSaveChange(model.oid, msg).flatMap { response =>
          Client[PublicationFunctions].changePublishedModels().call().flatMap { _ =>
            if (publishable() && currentPermLevel(readPermInfo, instanceReadVar(), false) == SecurityInherited) {
              // This is the default situation -- Instances inherit their permissions from the Space. But that
              // is *not* what we want for Publishables: they want to default to Members:
              val readPath = Editing.propPath(readPermId, Some(model.oid))
              val readMsg = ChangePropertyValue(readPath, List(std.security.members.oid.underlying))
              // Change the permission to Members...
              InputGadget.doSaveChange(instanceThing, readMsg).flatMap { response =>
                // ... fetch the new permissions...
                Client[SecurityFunctions].permsFor(model).call().map { newPerms =>
                  // ... and update the Var:
                  thingPerms() = newPerms
                }
              }
            } else {
              Future.successful(())
            }
          }
        }
      }
      guts = 
        div(
          h1(
            s"Publication Info for ${model.displayName}",
            querkiButton("Done")( 
              id:="_publishDone",
              href:=Editing.modelDesignerFactory.pageUrl(model)
            )
          ),
          p(new RxCheckbox(publishable, s" Instances of ${model.displayName} should be Publishable")),
          div(cls:="col-md-12", display:=Rx { if (publishable()) "block" else "none" },
            h3(cls:="col-md-12", "Who Can Read Instances Before Publication"),
            new OnePerm(
              instanceThing, 
              readPermInfo, 
              instanceReadVar,
              false,
              this),
            h3(cls:="col-md-12", "Who Can Read Instances After Publication"),
            new OnePerm(
              model, 
              afterPerm, 
              Rx { thingPerms().perms.find(_.permId == std.publication.canReadAfterPublishPerm.oid) },
              false,
              this)
          )
        )
    }
      yield PageContents(guts)
  }
}