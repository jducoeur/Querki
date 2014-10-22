package querki.pages

import scala.concurrent.Future

import scalatags.JsDom.all.{input => inp, _}

import querki.globals._

class ExplorePage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  val thingId = params.requiredParam("thingId")

  def pageContent = for {
    thingInfo <- DataAccess.getThing(thingId)
    guts = 
      div(
        p(b("Enter a QL expression below, and press Tab to see what it generates:")),
        
        div(id:="_exploreQueryRow", cls:="row-fluid",
          div(cls:="span3 _exploreSurround",
            p(inp(id:="_exploreThingName", tpe:="text", placeholder:=thingInfo.displayName),
              "-> [[")
          ),
          div(id:="_exploreQlInputDiv", cls:="span8",
            textarea(id:="_exploreQlInput", placeholder:="_foreachProperty", width:="100%")
          ),
          div(cls:="span1 _exploreSurround", "]]")
        ),
        
        p(b("Results:")),
        
        hr,
        
        div(id:="_results")
      )
    }
  	  yield PageContents(s"QL Explorer for ${thingInfo.displayName}", guts)

}
