package querki.pages

import scala.async.Async._
import scala.util.{Failure, Success}

import upickle._
import autowire._

import org.scalajs.dom
import org.scalajs.jquery._

import scalatags.JsDom.all._

import models.{Kind, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.{Gadget, QText, WrapperDiv}
import querki.comm._

class ThingPage(e:Ecology) extends Page(e) with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  
  type DetailsType = ThingPageDetails
  
  val thing = DataAccess.mainThing.get
  
  def title = thing.displayName
  
  def pageContent = {
    val renderedContent = new WrapperDiv
    
    async {
      val rendered = await(Client[ThingFunctions].renderThing(DataAccess.thingId).call())
	  renderedContent.replaceContents(new QText(rendered).render)
    }
    
    div(
      details.customHeader match {
        case Some(header) => new QText(header)
        case None => new StandardThingHeader(thing, this)
      },
      renderedContent(p("Loading..."))
    )
  }
}

class StandardThingHeader(thing:ThingInfo, page:Page)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val thingName = thing.displayName
  
  val modelOpt = DataAccess.mainModel
  
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
              oldEditButton
            },
            if (thing.isInstantiatable) {
              iconButton("plus-sign")(
                title:=s"Create a $thingName",
                href:=controllers.Application.doCreateThing2.spaceUrl(thing.urlName))
            },
            querkiButton(MSeq(icon("edit"), icon("edit"), icon("edit"), "..."))(
              title:=s"Edit all instances of $thingName",
              href:=controllers.Application.editInstances.spaceUrl(thing.urlName))
          )
        } else {
          // Not a Model
          MSeq(
            if (thing.isEditable) {
              if (thing.isTag || thing.kind == Kind.Property) {
                oldEditButton
              } else {
                iconButton("edit", Seq("_qlInvoke"))(
                  title:=s"Edit $thingName",
                  data("thingid"):=thing.urlName,
                  data("target"):="_topEdit",
                  data("ql"):="_edit",
                  href:="#")
              }
            },
            modelOpt match {
              case Some(model) if (model.isInstantiatable) => {
                querkiButton(MSeq(icon("plus-sign"), "..."))(
                  title:=s"Create another ${model.displayName}",
                  href:=controllers.Application.doCreateThing2.spaceUrl(model.urlName))
              }
              case None => {}
            }
          )
        }
      ),
      
      modelOpt match {
        case Some(model) => {
          p(cls:="_smallSubtitle _noPrint",
            "(OID: ", a(href:=controllers.Application.thing.spaceUrl(thing.oid), thing.oid),
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
