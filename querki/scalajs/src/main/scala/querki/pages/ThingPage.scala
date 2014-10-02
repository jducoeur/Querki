package querki.pages

import scala.async.Async._
import scala.util.{Failure, Success}

import upickle._
import autowire._

import org.scalajs.dom
import org.scalajs.jquery._

import scalatags.JsDom.all._

import models.Kind

import querki.globals._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.{Gadget, WrapperDiv}
import querki.comm._


class ThingPage(val ecology:Ecology) extends Page with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  
  val thing = DataAccess.mainThing.get
  
  def title = thing.displayName
  
  def pageContent = {
    val renderedContent = new WrapperDiv
    
    async {
      val rendered = await(Client[ThingFunctions].renderThing(DataAccess.thingId).call())
	  renderedContent.replaceContents(div(raw(rendered)).render)
    }
    
    div(
      new StandardThingHeader(ecology, thing),
      renderedContent(p("Loading..."))
    )
  }
}

class StandardThingHeader(val ecology:Ecology, thing:ThingInfo) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val thingName = thing.displayName
  
  val modelOpt = DataAccess.mainModel
  
  def MSeq(xs:Modifier*) = Vector[Modifier](xs)
  
  lazy val oldEditButton = 
    a(cls:="btn btn-mini btn-primary _noPrint querki-icon-button",
      title:=s"Edit $thingName",
      href:=controllers.Application.editThing(DataAccess.userName, DataAccess.spaceId, thing.urlName).url,
      i(cls:="icon-edit icon-white"))
  
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
              a(cls:="btn btn-mini btn-primary _noPrint querki-icon-button",
                title:=s"Create a $thingName",
                href:=controllers.Application.doCreateThing2(DataAccess.userName, DataAccess.spaceId, thing.urlName).url,
                i(cls:="icon-plus-sign icon-white"))
            },
            a(cls:="btn btn-mini btn-primary _noPrint querki-icon-button",
              title:=s"Edit all instances of $thingName",
              href:=controllers.Application.editInstances(DataAccess.userName, DataAccess.spaceId, thing.urlName).url,
              i(cls:="icon-edit icon-white"), i(cls:="icon-edit icon-white"), i(cls:="icon-edit icon-white"), "...")
          )
        } else {
          // Not a Model
          MSeq(
            if (thing.isEditable) {
              if (thing.isTag || thing.kind == Kind.Property) {
                oldEditButton
              } else {
                a(cls:="btn btn-mini btn-primary _noPrint _qlInvoke querki-icon-button",
                  title:=s"Edit $thingName",
                  data("thingid"):=thing.urlName,
                  data("target"):="_topEdit",
                  data("ql"):="_edit",
                  href:="#",
                  i(cls:="icon-edit icon-white"))
              }
            },
            modelOpt match {
              case Some(model) if (model.isInstantiatable) => {
                a(cls:="btn btn-mini btn-primary _noPrint querki-icon-button",
                  title:=s"Create another ${model.displayName}",
                  href:=controllers.Application.doCreateThing2(DataAccess.userName, DataAccess.spaceId, model.urlName).url,
                  i(cls:="icon-plus-sign icon-white"), "...")
              }
              case None => {}
            }
          )
        }
      ),
      modelOpt match {
        case Some(model) => {
          p(cls:="_smallSubtitle _noPrint",
            "(OID: ", a(href:=controllers.Application.thing(DataAccess.userName, DataAccess.spaceId, thing.oid).url, thing.oid),
            thing.linkName.map { linkName =>
              MSeq(", Link Name: ", a(href:=controllers.Application.thing(DataAccess.userName, DataAccess.spaceId, thing.urlName).url, linkName))
            },
            ", Model: ", a(href:=controllers.Application.thing(DataAccess.userName, DataAccess.spaceId, model.urlName).url, model.displayName),
            ")")
        }
        case None => {}
      }
    )
}
