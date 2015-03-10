package querki.pages

import scala.util.{Failure, Success}

import scala.scalajs.js
import upickle._
import autowire._

import org.scalajs.dom.{raw => dom}
import org.scalajs.jquery._

import scalatags.JsDom.all._
import scalatags.JsDom.tags2

import models.{Kind, Wikitext}

import querki.globals._

import querki.api.{StandardThings, ThingFunctions}
import querki.comm._
import querki.conversations.ConversationPane
import querki.data.ThingInfo
import querki.display.{Gadget, QLButtonGadget, QText, WrapperDiv}

class ThingPage(name:TID, params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]

  
  def pageContent = {
    // NOTE: doing this with async/await seems to swallow exceptions in Autowire:
    for {
      pageDetails:ThingPageDetails <- Client[ThingFunctions].getThingPage(name).call()
      standardThings <- DataAccess.standardThings
      rendered = pageDetails.rendered
      convPane = new ConversationPane(pageDetails.thingInfo, params.get("showComment"))
      dummy = {
        DataSetting.setThing(Some(pageDetails.thingInfo))
        DataSetting.setModel(pageDetails.modelInfo)
      }
      guts = 
        div(
          pageDetails.headers.map(raw(_)),
          if (!pageDetails.stylesheets.isEmpty)
            tags2.style(id:="_pageStyles", pageDetails.stylesheets.mkString("\n")),
          div(id:="_topEdit", display.none),
          pageDetails.customHeader match {
            case Some(header) => new QText(header)
            case None => new StandardThingHeader(pageDetails.thingInfo, this, standardThings)
          },
          new QText(rendered),
          convPane
        )
    }
      yield PageContents(pageDetails.thingInfo.displayName, guts)
  }
}

class StandardThingHeader(thing:ThingInfo, page:Page, standardThings:StandardThings)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  
  val thingName = thing.displayName
  
  val modelOpt = DataAccess.mainModel
  
  lazy val topEditButton =
    new QLButtonGadget(
    	iconButton("edit", Seq("_qlInvoke"))(
                  title:=s"Edit $thingName",
                  data("thingid"):=thing,
                  data("target"):="_topEdit",
                  data("ql"):="_edit",
                  href:=page.thingUrl(thing))
    )
  
  def doRender =
    div(cls:="page-header",
        
      h1(cls:="_defaultTitle", 
        thingName, " ",
        if (thing.isModel) {
          MSeq(
            if (thing.isEditable) {
              iconButton("edit")(
                title:=s"Design $thingName",
                href:=Editing.modelDesignerFactory.pageUrl(thing))
            },
            if (thing.isInstantiatable) {
              iconButton("plus")(
                title:=s"Create a $thingName",
                href:=Pages.createAndEditFactory.pageUrl(thing))
            },
            querkiButton(MSeq(icon("edit"), " ", icon("edit"), " ", icon("edit"), "..."))(
              title:=s"Edit all instances of $thingName",
              href:=Editing.editInstancesFactory.pageUrl(thing))
          )
        } else {
          // Not a Model
          MSeq(
            if (thing.isEditable) {
              if (thing.isTag) {
                iconButton("edit")(
                  title:=s"Make $thingName into a real Thing",
                  href:=
                    Pages.createAndEditFactory.pageUrl(
                      modelOpt.getOrElse(standardThings.basic.simpleThing),
                      (Editing.propPath(standardThings.basic.displayNameProp.oid) -> thingName)))
              } else if (thing.kind == Kind.Property) {
			    iconButton("edit")(
			      title:=s"Edit $thingName",
			      href:=Editing.advancedEditorFactory.pageUrl(thing))
              } else {
                topEditButton
              }
            }
          )
        },
        Gadget(iconButton("refresh")(title:="Refresh this page"), { e => 
          $(e).click({ evt:JQueryEventObject => PageManager.reload() }) 
        })
      ),
      
      modelOpt match {
        case Some(model) if (!thing.isTag) => {
          p(cls:="_smallSubtitle _noPrint",
            "(OID: ", a(href:=page.thingUrl(thing.oid), thing.oid.underlying),
            thing.linkName.map { linkName =>
              MSeq(", Link Name: ", a(href:=page.thingUrl(thing.urlName), linkName))
            },
            ", Model: ", a(href:=page.thingUrl(model), model.displayName),
            ")")
        }
        case Some(model) if (thing.isTag) => {
          p(cls:="_smallSubtitle _noPrint", 
            "(Tag based on ", thingLink(model), ")")
        }
        case None => {}
      }
    )
}
