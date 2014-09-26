package querki.pages

import scala.util.{Failure, Success}

import upickle._

import org.scalajs.dom
import org.scalajs.jquery._

import scalatags.JsDom.all._

import querki.globals._

import querki.comm._

class ThingPage(val ecology:Ecology, pickled:String) extends Page with EcologyMember {

  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val info = read[ThingPageInfo](pickled)
  
  def title = info.thing.displayName
  
  def pageContent = {
    controllers.ClientController.renderThing(DataAccess.userName, DataAccess.spaceId, DataAccess.thingId).callAjax().onComplete { result =>
      result match {
        case Success(pickled) => {
	      val rendered = read[RenderedThing](pickled)
	      val html = rendered.rendered
	      replaceContents(div(raw(html)).render)
        }
        case Failure(ex:PlayAjaxException) => {
	      replaceContents(
	        p(s"Got an error: ${ex.textStatus}").render
	      )          
        }
        case Failure(ex) => {}
      }
    }
    
    p("Loading...")
  }
}
