package querki.pages

import scala.concurrent.Future

import scalatags.JsDom.all.{input => inp, _}
import org.scalajs.dom
import org.scalajs.jquery._
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.display.{QText, WrapperDiv}
import querki.display.input.{InputGadget, ManifestItem, MarcoPoloInput, TagSetKind}

class ExplorePage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  
  val initialThingId = params.requiredParam("thingId")
  var chosenThingId:Option[String] = None
  def thingId = {
    chosenThingId.getOrElse(initialThingId)
  }

  class ThingSelect(mods:Modifier*) extends MarcoPoloInput("", false, TagSetKind.Link, mods) {
    override def onChange(q:String) = {}
    
    override def onSelect(item:ManifestItem) = {
      chosenThingId = Some(item.id)
      evaluate()
    }
  }
  
  class QLInput extends InputGadget[dom.HTMLTextAreaElement](ecology) {
    
    val default = "_foreachProperty"
    
    def doRender() = textarea(id:="_exploreQlInput", placeholder:=default, width:="100%")
    
    def value = {
      val raw = $(elem).value().asInstanceOf[String]
      if (raw.length > 0)
        raw
      else
        default
    }
    
    def hook() = {
      $(elem).keydown { (evt:JQueryEventObject) =>
        if (evt.which == 9) {
          evaluate();
          evt.preventDefault();
          false
        }  
      }
      $(elem).keyup { (evt:JQueryEventObject) =>
        if (evt.which == 9) {
          evt.preventDefault();
          false
        }  
      }
    }
  }
  
  def evaluate() = {
    println(s"Evaluating $thingId on ${qlInput.value}")
    Client[ThingFunctions].evaluateQL(thingId, qlInput.value).call().foreach { result =>
      val qtext = new QText(result)
      results.replaceContents(qtext.render)
    }
  }
  
  lazy val qlInput = new QLInput
  lazy val results = new WrapperDiv

  def pageContent = for {
    thingInfo <- DataAccess.getThing(thingId)
    guts = 
      div(
        p(b("Enter a QL expression below, and press Tab to see what it generates:")),
        
        div(id:="_exploreQueryRow", cls:="row-fluid",
          div(cls:="span3 _exploreSurround", p(new ThingSelect(id:="_exploreThingName", placeholder:=thingInfo.displayName), "-> [[")),
          div(id:="_exploreQlInputDiv", cls:="span8", qlInput),
          div(cls:="span1 _exploreSurround", "]]")
        ),
        
        p(b("Results:")),
        
        hr,
        
        results
      )
    }
  	  yield PageContents(s"QL Explorer for ${thingInfo.displayName}", guts)

}
