package querki.pages

import scala.util.{Failure, Success}

import scala.scalajs.js
import upickle._
import autowire._

import org.scalajs.{dom => fulldom}
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._

import scalatags.JsDom.all.{data => dta, _}
import scalatags.JsDom.tags2

import models.{Kind, Wikitext}

import querki.globals._

import querki.api.{ModelLoopException, ThingFunctions}
import querki.comm._
import querki.conversations.ConversationPane
import querki.data.ThingInfo
import querki.display.{QLButtonGadget, QText, QuerkiUIUtils, WrapperDiv}

class ThingPage(name:TID, params:ParamMap)(implicit val ecology:Ecology) extends Page() with QuerkiUIUtils {

  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
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
    
    val pageFut = Client[ThingFunctions].getThingPage(name, propOpt).call()
    pageFut.onFailure {
      case ModelLoopException() => StatusLine.showUntilChange("It appears you have a Model loop. Please go into the Advanced Editor and change models there.")
      case ex:Exception => StatusLine.showUntilChange(ex.getMessage)
      case _ => StatusLine.showUntilChange("Unexpected error")
    }
    
    for {
      pageDetails:ThingPageDetails <- pageFut
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

class StandardThingHeader(thing:ThingInfo, page:Page)(implicit val ecology:Ecology) 
  extends Gadget[dom.HTMLDivElement] with QuerkiUIUtils with EcologyMember 
{

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[querki.editing.Editing]
  lazy val History = interface[querki.history.History]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val Publication = interface[querki.publication.Publication]
  
  val thingName = thing.displayName
  val std = page.std
  
  val appPrefix =
    thing.importedFrom.map(spaceInfo => s"${spaceInfo.displayName}::").getOrElse("")
  
  val modelOpt = DataAccess.mainModel
  
  def viewingHistory = History.viewingHistory
  
  def isSpace = thing.kind == Kind.Space
  def isApp =
    if (isSpace) {
      DataAccess.space.map(_.isApp).getOrElse(false)
    } else
      false
  
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
  
  // Note that this is a Bootstrap button with a drop-down menu.
  // TODO: this probably wants to become a standard utility type.
  lazy val shareButton =
    div(cls:="btn-group querki-icon-button",
      button(
        tpe:="button", 
        cls:="btn btn-default btn-xs btn-primary _noPrint",
        dta.toggle:="dropdown",
        aria.haspopup:="true", aria.expanded:="false",
        i(cls:="fa fa-share-alt", aria.hidden:="true"), " ",
        span(cls:="caret"),
        title:="Share..."
      ),
      ul(cls:="dropdown-menu",
        li(a(
          href:=s"mailto:?subject=${thing.displayName}&body=${js.URIUtils.encodeURI(fulldom.window.location.href)}",
          target:="_blank",
          "Share via Email...")),
        if (isSpace && DataAccess.request.isOwner) {
          MSeq(
            li(a(href:=Pages.shareableLinkFactory.pageUrl(), "Get Shareable Link...")),
            li(a(href:=Pages.sharingFactory.pageUrl("tab" -> SharingPage.Tab.Invite.entryName), "Invite Members...")),
            li(a(href:=Pages.sharingFactory.pageUrl("tab" -> SharingPage.Tab.Members.entryName), "Show Members"))
          )
        }
      )
    )
    
  lazy val spacePublicationsButton = {
    div(cls:="btn-group querki-icon-button",
      button(
        tpe:="button",
        cls:=s"btn btn-xs btn-primary _noPrint",
        dta.toggle:="dropdown",
        aria.haspopup:="true", aria.expanded:="false",
        i(cls:="fa fa-rss", aria.hidden:="true"), " ",
        span(cls:="caret"),
        title:="This Space has Publications"
      ),
      ul(cls:="dropdown-menu",
        li(a(
          href:="#!recent-space-changes",
          "Recent Changes"))
      )
    )
  }
    
  lazy val publishButton = {
    val isPublished = Publication.isPublished(thing)
    val hasUnpublishedChanges = Publication.hasUnpublishedChanges(thing)
    val needsPublish = (!isPublished) || hasUnpublishedChanges
    val btnColor = if (!needsPublish) "btn-default" else "btn-warning"
    val btnTitle = 
      if (isPublished) {
        if (hasUnpublishedChanges)
          "This has not-yet-published changes"
        else
          "This has been published" 
      } else
        "This has not yet been published"
    
    div(cls:="btn-group querki-icon-button",
      button(
        tpe:="button",
        cls:=s"btn btn-xs $btnColor _noPrint",
        dta.toggle:="dropdown",
        aria.haspopup:="true", aria.expanded:="false",
        i(cls:="fa fa-rss", aria.hidden:="true"), " ",
        if (needsPublish) {
          span(cls:="caret")
        },
        title:=btnTitle
      ),
      if (needsPublish) {
        if (hasUnpublishedChanges) {
          ul(cls:="dropdown-menu",
          MSeq(
            li(a(
              href:="#",
              onclick:={ () => Publication.update(thing, false) },
              "Publish an Update")),
            li(a(
              href:="#",
              onclick:={ () => Publication.update(thing, true) },
              "Publish a Minor Update"))
            )          
          )
        } else {
          ul(cls:="dropdown-menu",
            li(a(
              href:="#",
              onclick:={ () => Publication.publish(thing) },
              "Publish"))
          )
        }
      }
    )
  }
  
  def doRender =
    div(cls:="page-header",
        
      h1(cls:="_defaultTitle", 
        if (isApp)
          b("App: "),
        raw(appPrefix),
        raw(thingName), " ",
        if (isSpace)
          querkiButton(faIcon("info"))(
            title:=s"Info about $thingName",
            id:="_infoButton",
            href:=Pages.infoFactory.pageUrl()),
            
        if (!viewingHistory) {
          // The "editing" buttons:
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
                } else if (isSpace) {
                  iconButton("edit")(
                    title:=s"Edit Space Info",
                    href:=Editing.editSpaceInfoFactory.pageUrl())
                } else if (thing.kind == Kind.Property) {
        			    iconButton("edit")(
        			      title:=s"Edit $thingName",
        			      href:=Editing.advancedEditorFactory.pageUrl(thing))
                } else if (thing.hasFlag(std.publication.publishableProp)) {
                  // TODO: for now, Publishables have to use the Advanced Editor. This should change, and
                  // at that point this can go back to using the topEditButton:
        			    iconButton("edit")(
        			      title:=s"Edit $thingName",
        			      href:=Editing.advancedEditorFactory.pageUrl(thing))                  
                } else {
                  topEditButton
                }
              },
              if (isSpace && Publication.spaceHasPublications(thing)) {
                spacePublicationsButton
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
          }
        },
        shareButton,
        // Editors see the Publish button on Publishable Instances:
        // TODO: this button should also show for Models, but has a very different menu.
        if (Publication.isPublishable(thing) && thing.isEditable) {
          publishButton
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
