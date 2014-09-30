package querki.pages

import scala.util.{Failure, Success}

import upickle._
// TODO: this is needed in order to get at autowire.clientFutureCallable(), which makes call() available:
import autowire._

import org.scalajs.dom
import org.scalajs.jquery._

import scalatags.JsDom.all._

import querki.globals._

import querki.api.ThingFunctions
import querki.client.MyClient
import querki.comm._

class ThingPage(val ecology:Ecology, pickled:String) extends Page with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val info = read[ThingPageInfo](pickled)
  
  def title = info.thing.displayName
  
  def pageContent = {
    Client[ThingFunctions].renderThing(DataAccess.thingId).call().foreach { rendered =>
	  replaceContents(div(raw(rendered)).render)      
    }
    
    p("Loading...")
  }
}
