package querki.publication

import scalatags.JsDom.all._
import autowire._
import rx._
import org.querki.gadgets._

import querki.ecology._
import querki.editing.EditFunctions
import EditFunctions.ChangePropertyValue
import querki.globals._
import querki.pages._
import querki.display.QText
import querki.display.input.InputGadget
import querki.display.rx._
import querki.security.{LevelMap, OnePerm, SecurityFunctions}
import SecurityFunctions._

class EditPublicationPage(params: ParamMap)(implicit val ecology: Ecology) extends Page("editPublish") with LevelMap {
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]

  val modelId = TID(params.requiredParam("modelId"))

  lazy val publishPermId = std.publication.canPublishPerm.oid

  def pageContent = {
    for {
      model <- DataAccess.getThing(modelId)
      // TODO: provide some more-useful feedback if this sanity-check ever fails:
      if (model.isModel)
      allPerms <- Client[SecurityFunctions].getAllPerms().call()
      publishPermInfo = allPerms.find(_.id == publishPermId).get
      publishPerm <- Client[SecurityFunctions].getOnePerm(std.publication.canPublishPerm).call()
      thingPerms <- Client[SecurityFunctions].permsFor(DataAccess.space.get).call().map(Var(_))
      instancePublishVar = Rx { thingPerms().perms.find(_.permId == publishPermId) }
      publishable = Var[Boolean](model.hasFlag(std.publication.publishableProp))
      watcher = publishable.triggerLater {
        val path = Editing.propPath(std.publication.publishableProp, Some(model.oid))
        val msg = ChangePropertyValue(path, List(publishable.now.toString))
        InputGadget.doSaveChange(model.oid, msg).flatMap { response =>
          Client[PublicationFunctions].changePublishedModels().call()
        }
      }
      guts =
        div(
          h1(
            s"Publication Info for ${model.displayName}",
            querkiButton("Done")(
              id := "_publishDone",
              href := Editing.modelDesignerFactory.pageUrl(model)
            )
          ),
          p(new RxCheckbox(publishable, s" Instances of ${model.displayName} should be Publishable")),
          div(
            cls := "col-md-12",
            display := Rx { if (publishable()) "block" else "none" },
            h3(cls := "col-md-12", "Who Can Publish"),
            QText("""These people currently can *both* read Instances before they are Published, and can Publish/Update Instances to
                    |make them more publicly visible. (These will probably be broken into separate permissions sometime later.) Note
                    |that this Permission is Space-wide, and applies to all Publishable Models.""".stripMargin),
            new OnePerm(
              DataAccess.space.get,
              publishPerm,
              instancePublishVar,
              true,
              this
            )
          )
        )
    } yield PageContents(guts)
  }
}
