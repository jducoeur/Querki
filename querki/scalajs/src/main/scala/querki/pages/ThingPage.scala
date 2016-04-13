package querki.pages

import scala.util.{Failure, Success}

import scala.scalajs.js
import upickle._
import autowire._

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._

import scalatags.JsDom.all._
import scalatags.JsDom.tags2

import models.{Kind, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.comm._
import querki.conversations.ConversationPane
import querki.data.ThingInfo
import querki.display.{Gadget, QLButtonGadget, QText, WrapperDiv}

class ThingPage(name:TID, params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]
  
  // If there is a "prop" parameter, that declares a Property that we're trying to render
  // instead of Default View:
  lazy val propOpt = params.get("prop").map(TID(_))

  override def refresh() = PageManager.reload()
  
  def pageContent = {
    // We do this in parallel, so that we can update the menu and title quickly if the
    // full page takes a long time or hits an error.
    // TODO: in principle, this is suspicious -- it's very side-effecty, and subject to
    // race conditions if the user changes pages quickly. Think about whether there is
    // a better way to do it. This code is *not* crucial, except in the edge case where
    // getThingPage fails or is slow.
    for {
      info <- Client[ThingFunctions].getThingInfo(name).call()
    }
    {
      DataSetting.setThing(Some(info))
      PageManager.update(info.displayName)
    }
    
    for {
      pageDetails:ThingPageDetails <- Client[ThingFunctions].getThingPage(name, propOpt).call()
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
            case None => new StandardThingHeader(pageDetails.thingInfo, this)
          },
          new QText(rendered),
          convPane
        )
    }
      yield PageContents(pageDetails.thingInfo.unsafeName, guts)
  }
}

class StandardThingHeader(thing:ThingInfo, page:Page)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  
  val thingName = thing.displayName
  val std = page.std
  
  val modelOpt = DataAccess.mainModel
  
  lazy val topEditButton =
    new QLButtonGadget(
    	iconButton("edit", Seq("_qlInvoke"))(
                  title:=s"Edit $thingName",
                  id:="_thingEdit",
                  data("thingid"):=thing,
                  data("target"):="_topEdit",
                  data("noicon"):="true",
                  data("ql"):=EditQL(),
                  href:=page.thingUrl(thing))
    )
  
  def doRender =
    div(cls:="page-header",
        
      h1(cls:="_defaultTitle", 
        raw(thingName), " ",
        if (thing.isModel) {
          MSeq(
            if (thing.isEditable) {
              iconButton("edit")(
                title:=s"Design $thingName",
                id:="_thingEdit",
                href:=Editing.modelDesignerFactory.pageUrl(thing))
            },
            if (thing.isInstantiatable) {
              iconButton("plus")(
                title:=s"Create a $thingName",
                href:=Pages.createAndEditFactory.pageUrl(thing))
            },
            if (thing.isEditable || thing.isInstantiatable) {
              querkiButton(MSeq(icon("edit"), " ", icon("edit"), " ", icon("edit"), "..."))(
                title:=s"Edit all instances of $thingName",
                href:=Editing.editInstancesFactory.pageUrl(thing))
            }
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
                      modelOpt.getOrElse(std.basic.simpleThing),
                      (Editing.propPath(std.basic.displayNameProp.oid) -> thingName),
                      "reifyTag" -> "true"))
              } else if (thing.kind == Kind.Property || thing.kind == Kind.Space) {
      			    iconButton("edit")(
      			      title:=s"Edit $thingName",
      			      href:=Editing.advancedEditorFactory.pageUrl(thing))
              } else {
                topEditButton
              }
            },
            modelOpt match {
              case Some(model) if (model.isInstantiatable) => {
                querkiButton(MSeq(icon("plus-sign"), "..."))(
                  title:=s"Create another ${model.displayName}",
                  href:=Pages.createAndEditFactory.pageUrl(model))
              }
              case _ => {}
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
            "(OID: ", a(href:=page.thingUrl(thing.oid), id:="_thingOID", thing.oid.underlying),
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
