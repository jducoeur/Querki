package querki.display.input

import scala.async.Async._
import upickle._
import autowire._

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._
import querki.display._

/**
 * Base class for input controls. When you create a new concrete class, make sure to add it to
 * InputGadgets.registry.
 */
abstract class InputGadget(val ecology:Ecology) extends Gadget[dom.Element] with EcologyMember {
  
  type elemType <: dom.Element
  
  def rawElement:dom.Element
  
  lazy val element = rawElement.asInstanceOf[elemType]
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  /**
   * Hook whatever events are appropriate for this Gadget.
   */
  def hook():Unit
  
  /**
   * Records a change that the user has made. This should be called by the specific Gadget when
   * appropriate.
   * 
   * @param vs The new values of this field. Note that this is plural, since some Gadgets are inherently
   *    List/Set based. Conventional single-valued fields should just pass in Some(v). Values should be
   *    in whatever serialized form the server-side PType expects.
   */
  def saveChange(vs:List[String]) = {
    val path = $(element).attr("name")
    println(s"Sending new values for $path: $vs")
    async {
      val response = await(Client[EditFunctions].alterProperty(DataAccess.thingId, path, ChangePropertyValue(vs)).call())
	  println(s"Result was $response")
	  response match {
        case PropertyChanged => $(element).trigger("savecomplete")
        case PropertyChangeError(msg) => $(element).trigger("saveerror")
      }
    }    
  }
}
