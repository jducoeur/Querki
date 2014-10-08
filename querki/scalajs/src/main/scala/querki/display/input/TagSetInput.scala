package querki.display.input

import scala.scalajs.js
import js.Dynamic.{literal => lit}

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

trait ManifestFacade extends JQuery {
  def manifest(config:Any):this.type = ???
}
object ManifestFacade {
  implicit def jq2Manifest(jq:JQuery):ManifestFacade = jq.asInstanceOf[ManifestFacade]
}
import ManifestFacade._

trait ManifestItem extends js.Object {
  def display:String = ???
  def id:js.UndefOr[String] = ???
}
  
class TagSetInput(val rawElement:dom.Element)(implicit e:Ecology) extends InputGadget(e) {
  
  type elemType = dom.HTMLInputElement

  // Required data attributes of any Tag Set Input:
  lazy val isNames = $(element).data("isnames").asInstanceOf[Boolean]
  lazy val initialValuesJs = $(element).data("current")
  lazy val propId = $(element).data("prop").asInstanceOf[String]
  
  // TEMP: while we're still using the old MarcoPolo entry points:
  lazy val entryPoint = if (isNames) "getTags" else "getLinks"
  lazy val required = if (isNames) false else true
  lazy val typeName = if (isNames) "tag" else "thing"
    
    println(s"isNames = $isNames; required = $required")
  
  // The constructor for the Manifest object itself. This prompts you when you start typing,
  // using MarcoPolo, and organizes results into a nice list.
  $(element).manifest(lit(
    // TODO: marcoPolo should be going through the ClientController!
    marcoPolo = lit(
      url = s"$entryPoint?propId=$propId",
      minChars = 1,
      required = required,
      formatData = { data:js.Object => data },
      formatItem = { (data:ManifestItem) => data.display },
      formatNoResults = { (q:String) => s"No existing $typeName with <b>$q</b> found." }
    ),
    // Yes, these two are horrible, but represent an unfortunate quirk of Manifest: these sometimes get called with
    // items, sometimes with Strings:
    formatDisplay = { (data:js.Any) => if (data.isInstanceOf[js.prim.String]) data else data.asInstanceOf[ManifestItem].display },
    formatValue = { (data:js.Any) => if (data.isInstanceOf[js.prim.String]) data else data.asInstanceOf[ManifestItem].id },
//    separator = Array[Char](13, ','),
    values = initialValuesJs,
    required = required
  ))
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
//    $(element).change({ event:JQueryEventObject =>
//      println("Have changed the Manifest!")
//      saveChange(List(element.value))
//    })
  }
  
  def doRender() =
    input(cls:="_textEdit", tpe:="text")

}