package querki.pages

import autowire._
import scalatags.JsDom.all._

import querki.globals._

import querki.api.ThingFunctions

class ViewPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  
  lazy val name = params("thingId")

  def pageContent = {
    for {
      thing <- DataAccess.getThing(name)
      propsRaw <- Client[ThingFunctions].getProperties(name).call()
      props = propsRaw.sortBy(_.name)
      pageTitle = s"Viewing Source of ${thing.importedFrom.map(_.displayName + "::").getOrElse("")}${thing.displayName}"
      guts = 
        div(
          div(cls:="page-header",
            h1(cls:="_defaultTitle", pageTitle, " ", a(cls:="cancelButton btn", href:=thingUrl(thing), "Done")),
            DataAccess.mainModel match {
              case Some(model) => p(cls:="_smallSubtitle", "(", thingLink(model), ")")
              case _ => {}
            }
          ),
          
          div(cls:="row-fluid",
            dl(
              for { 
                prop <- props
                tt = prop.tooltip.map(_.raw.toString).map(title:=_)
                label = prop.prompt.map(_.display.toString).getOrElse(prop.name)
              }
                yield MSeq(
                  dt(cls:="control-label _withTooltip", if (tt.isDefined) { tt.get }, label),
                  dd(raw(prop.renderedV.display.toString))
                )
            )
          )
        )
    }
  	  yield PageContents(pageTitle, guts)
  }
}
