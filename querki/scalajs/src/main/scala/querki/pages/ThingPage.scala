package querki.pages

import scala.util.{Failure, Success}

import upickle._
import autowire._

import org.scalajs.dom 
import org.scalajs.jquery._

import scalatags.JsDom.all._

import models.{Kind, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.comm._
import querki.conversations.ConversationPane
import querki.data.ThingInfo
import querki.display.{Gadget, QLButtonGadget, QText, WrapperDiv}

class ThingPage(name:String, params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]
  
  def pageContent = {
    // NOTE: doing this with async/await seems to swallow exceptions in Autowire:
    for {
      pageDetails:ThingPageDetails <- Client[ThingFunctions].getThingPage(name).call()
      rendered = pageDetails.rendered
      dummy = {
        DataSetting.setThing(Some(pageDetails.thingInfo))
        DataSetting.setModel(pageDetails.modelInfo)
      }
      guts = 
        div(
          div(id:="_topEdit", display.none),
          pageDetails.customHeader match {
            case Some(header) => new QText(header)
            case None => new StandardThingHeader(pageDetails.thingInfo, this)
          },
          new QText(rendered),
          new ConversationPane(pageDetails.thingInfo)
        )
    }
      yield PageContents(pageDetails.thingInfo.displayName, guts)
  }
}

class StandardThingHeader(thing:ThingInfo, page:Page)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val Pages = interface[querki.pages.Pages]
  
  val thingName = thing.displayName
  
  val modelOpt = DataAccess.mainModel
  
  lazy val topEditButton =
    new QLButtonGadget(
    	iconButton("edit", Seq("_qlInvoke"))(
                  title:=s"Edit $thingName",
                  data("thingid"):=thing.urlName,
                  data("target"):="_topEdit",
                  data("ql"):="_edit",
                  href:=page.thingUrl(thing))
    )
  
  lazy val oldEditButton = 
    iconButton("edit")(
      title:=s"Edit $thingName",
      href:=controllers.Application.editThing.spaceUrl(thing.urlName))
  
  def doRender =
    div(cls:="page-header",
        
      h1(cls:="_defaultTitle", 
        thingName, " ",
        if (thing.isModel) {
          MSeq(
            if (thing.isEditable) {
              iconButton("edit")(
                title:=s"Design $thingName",
                href:=Editing.modelDesignerFactory.pageUrl(("modelId" -> thing.urlName)))
            },
            if (thing.isInstantiatable) {
              iconButton("plus-sign")(
                title:=s"Create a $thingName",
                href:=Pages.createAndEditFactory.pageUrl(("model" -> thing.urlName)))
            },
            querkiButton(MSeq(icon("edit"), icon("edit"), icon("edit"), "..."))(
              title:=s"Edit all instances of $thingName",
              href:=Editing.editInstancesFactory.pageUrl(("modelId" -> thing.urlName)))
          )
        } else {
          // Not a Model
          MSeq(
            if (thing.isEditable) {
              if (thing.isTag || thing.kind == Kind.Property) {
                oldEditButton
              } else {
                topEditButton
              }
            },
            modelOpt match {
              case Some(model) if (model.isInstantiatable) => {
                querkiButton(MSeq(icon("plus-sign"), "..."))(
                  title:=s"Create another ${model.displayName}",
                  href:=Pages.createAndEditFactory.pageUrl(("model" -> model.urlName)))
              }
              case _ => {}
            }
          )
        }
      ),
      
      modelOpt match {
        case Some(model) => {
          p(cls:="_smallSubtitle _noPrint",
            "(OID: ", a(href:=page.thingUrl(thing.oid), thing.oid),
            thing.linkName.map { linkName =>
              MSeq(", Link Name: ", a(href:=page.thingUrl(thing), linkName))
            },
            ", Model: ", a(href:=page.thingUrl(model), model.displayName),
            ")")
        }
        case None => {}
      }
    )
}
