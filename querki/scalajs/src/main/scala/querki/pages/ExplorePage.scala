package querki.pages

import scala.concurrent.Future

import scalatags.JsDom.all.{input => inp, _}
import org.scalajs.dom.{raw => dom}
import org.scalajs.jquery._
import autowire._
import rx._

import querki.globals._

import models.Wikitext
import querki.api.ThingFunctions
import querki.display.{ButtonGadget, ButtonKind, QText, WrapperDiv}
import querki.display.input.{InputGadget, ManifestItem, MarcoPoloInput, TagSetKind}

class ExplorePage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val Client = interface[querki.client.Client]
  
  val initialThingId = TID(params.requiredParam("thingId"))
  var chosenThingId:Option[TID] = None
  def thingId = {
    chosenThingId.getOrElse(initialThingId)
  }

  class ThingSelect(mods:Modifier*) extends MarcoPoloInput("", false, TagSetKind.Link, mods) {
    override def onChange() = {}
    
    override def onSelect(item:ManifestItem) = {
      chosenThingId = Some(TID(item.id))
      evaluate()
    }
  }
  
  class QLInput extends InputGadget[dom.HTMLTextAreaElement](ecology) {
    
    val default = "_foreachProperty"
    
    def doRender() = textarea(id:="_exploreQlInput", cls:="form-control", placeholder:=default, width:="100%")
    
    def values = List(value)
    
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
      rawResults() = result
      val qtext = new QText(result)
      results.replaceContents(qtext.render)
      InputGadgets.hookPendingGadgets()
    }
  }
  
  lazy val qlInput = new QLInput
  lazy val results = new WrapperDiv
  lazy val rawResults = Var[Wikitext](Wikitext.empty)
  
  lazy val ReifyButton = new ButtonGadget(ButtonKind.Normal, "Make a Page")({
    val createFut = for {
      std <- DataAccess.standardThings
      createPage <- Pages.createAndEditFactory.showPage(std.basic.simpleThing)
      // TODO: we could get rid of this asInstanceOf by tweaking the type signature of showPage?
      dummy = createPage.asInstanceOf[CreateAndEditPage].setValue(std.basic.defaultView, s"[[${qlInput.value}]]")
    }
      yield createPage
  })
  
  lazy val SaveButton = new ButtonGadget(ButtonKind.Normal, "Save Results")({
    val saveFut = for {
      std <- DataAccess.standardThings
      createPage <- Pages.createAndEditFactory.showPage(std.basic.simpleThing)
      dummy = createPage.asInstanceOf[CreateAndEditPage].setValue(std.basic.defaultView, s"${rawResults().plaintext}")
    }
      yield createPage
  })

  def pageContent = for {
    thingInfo <- DataAccess.getThing(thingId)
    guts = 
      div(
        p(b("Enter a QL expression below, and press Tab to see what it generates:")),
        
        div(id:="_exploreQueryRow", cls:="row",
          div(cls:="col-md-2 _exploreSurround", new ThingSelect(id:="_exploreThingName", placeholder:=thingInfo.displayName)),
          div(cls:="col-md-1 _exploreSurround", b("-> [[")),
          div(id:="_exploreQlInputDiv", cls:="col-md-7", qlInput),
          div(cls:="col-md-1 _exploreSurround", b("]]"))
        ),
        
        p(ReifyButton),
        
        p(b("Results:")),
        
        hr,
        
        results,
        
        p(SaveButton)
      )
    }
  	  yield PageContents(s"QL Explorer for ${thingInfo.displayName}", guts)

}
