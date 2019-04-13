package querki.pages

import scala.util.{Failure, Success}

import scala.scalajs.js
import upickle._
import autowire._

import org.scalajs.{dom => fulldom}
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import org.querki.gadgets._

import scalatags.JsDom.all.{data => dta, _}
import scalatags.JsDom.tags2

import models.{Kind, Wikitext}

import querki.globals._

import querki.api.{ModelLoopException, ThingFunctions, UnknownThingException}
import querki.comm._
import querki.conversations.ConversationPane
import querki.data.ThingInfo
import querki.display.{QLButtonGadget, QText, QuerkiUIUtils, WrapperDiv}
import querki.security.SharingPage

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
      case UnknownThingException(thingId) => 
        PageManager.showRoot().map { page =>
          page.flash(true,
            s"There is no Thing with the ID $thingId"
          )
        }
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
  def modelIsSimpleThing = modelOpt.map(_.is(std.basic.simpleThing)).getOrElse(false)
  
  def viewingHistory = History.viewingHistory
  
  def isSpace = thing.kind == Kind.Space
  def isApp =
    if (isSpace) {
      DataAccess.space.map(_.isApp).getOrElse(false)
    } else
      false
      
  def spaceOpt = DataAccess.space
  lazy val hasExplore = spaceOpt.map { space =>
    space.permissions.contains(std.roles.canExplorePerm)
  }.getOrElse(false)
  
  lazy val canShare = spaceOpt.map { space =>
    space.permissions.contains(std.roles.canManageSecurityPerm)
  }.getOrElse(false)
    
  def editButton(addlCls:Seq[String] = Seq.empty) = faIconButton("pencil", addlCls)
  
  lazy val topEditButton =
    new QLButtonGadget(
      editButton(Seq("_qlInvoke"))(
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
        cls:="btn btn-default btn-sm _noPrint",
        dta.toggle:="dropdown",
        aria.haspopup:="true", aria.expanded:="false",
        faIcon("share-alt"), " ",
        span(cls:="caret"),
        title:="Share..."
      ),
      ul(cls:="dropdown-menu",
        li(a(
          href:=s"mailto:?subject=${thing.displayName}&body=${js.URIUtils.encodeURI(fulldom.window.location.href)}",
          target:="_blank",
          "Share via Email...")),
        if (isSpace && canShare) {
          MSeq(
            li(a(href := Pages.sharingFactory.pageUrl("tab" -> querki.security.SharingPage.Tab.CustomRoles.entryName), "Get Shareable Link...")),
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
        cls:=s"btn btn-sm _noPrint",
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
      if (needsPublish) {
        if (isPublished)
          "This has not-yet-published changes"
        else
          "This has not yet been published"
      } else
        "This has been published" 
    
    div(cls:="btn-group querki-icon-button",
      button(
        tpe:="button",
        cls:=s"btn btn-sm $btnColor _noPrint",
        dta.toggle:="dropdown",
        aria.haspopup:="true", aria.expanded:="false",
        faIcon("rss"), " ",
        if (needsPublish) {
          span(cls:="caret")
        },
        title:=btnTitle
      ),
      if (needsPublish) {
        ul(cls:="dropdown-menu",
          if (isPublished) {
            MSeq(
              li(a(
                onclick:={ () => Publication.update(thing, false, true) },
                "Publish an Update")),
              li(a(
                onclick:={ () => Publication.update(thing, true, true) },
                "Publish a Minor Update"))    
            )
          } else {
              li(a(
                onclick:={ () => Publication.publish(thing, true) },
                "Publish"))
          },
          li(a(
            onclick:={ () => Publication.discardChanges(thing, true) },
            "Discard Unpublished Changes"))
        )
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
          plainQuerkiButton(faIcon("cog"))(
            title:=s"Info about $thingName",
            id:="_infoButton",
            href:=Pages.infoFactory.pageUrl()),
            
        if (!viewingHistory) {
          // The "editing" buttons:
          if (thing.isModel) {
            MSeq(
              if (thing.isEditable) {
                editButton()(
                  title:=s"Design $thingName",
                  id:="_thingEdit",
                  href:=Editing.modelDesignerFactory.pageUrl(thing))
              },
              if (thing.isInstantiatable) {
                faIconButton("plus")(
                  title:=s"Create a $thingName",
                  href:=Pages.createAndEditFactory.pageUrl(thing))
              },
              if (thing.isEditable || thing.isInstantiatable) {
                plainQuerkiButton(MSeq(faIcon("pencil"), " ", faIcon("pencil"), " ", faIcon("pencil"), "..."))(
                  title:=s"Edit all instances of $thingName",
                  href:=Editing.editInstancesFactory.pageUrl(thing))
              }
            )
          } else {
            // Not a Model
            MSeq(
              if (thing.isEditable) {
                if (thing.isTag) {
                  modelOpt match {
                    case Some(modelId) if (!modelIsSimpleThing) => {
                      editButton()(
                        title:=s"Make $thingName into a real Thing",
                        href:=
                          Pages.createAndEditFactory.pageUrl(
                            modelId,
                            (Editing.propPath(std.basic.displayNameProp.oid) -> thingName),
                            "reifyTag" -> "true"))
                    }
                    case _ => {
                      // If no Model is known for the Tag, then ask:
                      editButton()(
                        title:=s"Make $thingName into a real Thing",
                        href:=
                          Pages.createAndEditFactory.pageUrl(
                            (Editing.propPath(std.basic.displayNameProp.oid) -> thingName),
                            "reifyTag" -> "true"))
                    }
                  }

                } else if (isSpace) {
                  editButton()(
                    title:=s"Edit Space Info",
                    href:=Editing.editSpaceInfoFactory.pageUrl())
                } else if (thing.kind == Kind.Property) {
                  editButton()(
                    title:=s"Edit $thingName",
                    href:=Editing.advancedEditorFactory.pageUrl(thing))
                } else if (thing.hasFlag(std.publication.publishableProp)) {
                  // TODO: for now, Publishables have to use the Advanced Editor. This should change, and
                  // at that point this can go back to using the topEditButton:
                  editButton()(
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
                  plainQuerkiButton(MSeq(faIcon("plus"), faIcon("ellipsis-h")))(
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
        Gadget(faIconButton("refresh")(title:="Refresh this page"), { e => 
          $(e).click({ evt:JQueryEventObject => PageManager.reload() }) 
        })
      ),
      
      if (hasExplore) {
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
      }
    )
}
