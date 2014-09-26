package querki.pages

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
    val request:PlayCall = controllers.ClientController.renderThing(DataAccess.userName, DataAccess.spaceId, DataAccess.thingId)
    val deferred = request.ajax().asInstanceOf[JQueryDeferred]
    deferred.done((data:String, textStatus:String, jqXHR:JQueryDeferred) => println(s"AJAX success: got $data"))
    deferred.fail((jqXHR:JQueryDeferred, textStatus:String, errorThrown:String) => println(s"Got an error"))
    
    raw(info.rendered)
  }
}
