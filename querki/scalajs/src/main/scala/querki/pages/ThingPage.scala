package querki.pages

import scala.async.Async._
import scala.util.{Failure, Success}

import upickle._
import autowire._

import org.scalajs.dom
import org.scalajs.jquery._

import scalatags.JsDom.all._

import querki.globals._

import querki.api.ThingFunctions
import querki.comm._

class ThingPage(val ecology:Ecology, pickled:String) extends Page with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val info = read[ThingPageInfo](pickled)
  
  def title = info.thing.displayName
  
  def pageContent = {
    async {
      val rendered = await(Client[ThingFunctions].renderThing(DataAccess.thingId).call())
	  replaceContents(div(raw(rendered)).render)
    }
    
    p("Loading...")
  }
}
