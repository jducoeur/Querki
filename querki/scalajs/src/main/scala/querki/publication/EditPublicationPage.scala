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

class EditPublicationPage(params:ParamMap)(implicit val ecology:Ecology) 
  extends Page("editPublish") with LevelMap
{
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]
  
  val modelId = TID(params.requiredParam("modelId"))
  
  def pageContent = {
    for {
      model <- DataAccess.getThing(modelId)
      // TODO: provide some more-useful feedback if this sanity-check ever fails:
      if (model.isModel)
      allPerms <- Client[SecurityFunctions].getAllPerms().call()
      afterPerm <- Client[SecurityFunctions].getOnePerm(std.publication.canReadAfterPublishPerm).call()
      thingPerms <- Client[SecurityFunctions].permsFor(model).call()
      instanceThing = thingPerms.instancePermThing.getOrElse(model)
      publishable = Var[Boolean](model.hasFlag(std.publication.publishableProp))
      watcher = Obs(publishable, skipInitial=true) {
        val path = Editing.propPath(std.publication.publishableProp, Some(model.oid))
        val msg = ChangePropertyValue(path, List(publishable().toString))
        InputGadget.doSaveChange(model.oid, msg).flatMap { response =>
          Client[PublicationFunctions].changePublishedModels().call().map { _ =>
            // Anything we should do here?
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
              allPerms.find(_.id == std.security.canReadPerm.oid).get, 
              thingPerms.instancePerms.find(_.permId == std.security.canReadPerm.oid),
              false,
              this),
            h3(cls:="col-md-12", "Who Can Read Instances After Publication"),
            new OnePerm(
              model, 
              afterPerm, 
              thingPerms.perms.find(_.permId == std.publication.canReadAfterPublishPerm.oid),
              false,
              this)
          )
        )
    }
      yield PageContents(guts)
  }
}