package querki.display.input

import scala.scalajs.js
import js.JSConverters._

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.comm._

trait ManifestFacade extends JQuery {
  def manifest(cmd:String):Any = ???
  def manifest(cmd:String, params:js.Object):Any = ???
  def manifest(config:js.Object):this.type = ???
}
object ManifestFacade {
  implicit def jq2Manifest(jq:JQuery):ManifestFacade = jq.asInstanceOf[ManifestFacade]
}
import ManifestFacade._

trait ManifestItem extends js.Object {
  def display:String = ???
  def id:String = ???
}
  
class TagSetInput(val rawElement:dom.Element)(implicit e:Ecology) extends InputGadget(e) {
  
  type elemType = dom.HTMLInputElement

  lazy val controllers = interface[querki.comm.ApiComm].controllers
  
  // Required data attributes of any Tag Set Input:
  lazy val isNames = $(element).data("isnames").asInstanceOf[Boolean]
  lazy val initialValuesJs = $(element).data("current")
  lazy val propId = $(element).data("prop").asInstanceOf[String]
  
  lazy val required = if (isNames) false else true
  lazy val typeName = if (isNames) "tag" else "thing"
    
  def stringOrItem(data:js.Any)(f:ManifestItem => String) = {
    if (data.isInstanceOf[js.prim.String]) 
      data 
    else 
      f(data.asInstanceOf[ManifestItem])
  }
  
  def values = { 
    $(element).manifest("values").asInstanceOf[js.Array[String]].toList
  }
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    // The constructor for the Manifest object itself. This prompts you when you start typing,
    // using MarcoPolo, and organizes results into a nice list.
    $(element).manifest(lit(
      marcoPolo = lit(
        url = controllers.ClientController.marcoPolo.spaceUrl(propId),
        minChars = 1,
        required = required,
        formatData = { data:js.Object => data },
        formatItem = { (data:ManifestItem) => data.display },
        formatNoResults = { (q:String) => s"No existing $typeName with <b>$q</b> found." }
      ),
      // Yes, these two are horrible, but represent an unfortunate quirk of Manifest: these sometimes get called with
      // items, sometimes with Strings:
      formatDisplay = { (data:js.Any) => stringOrItem(data)(_.display) },
      formatValue = { (data:js.Any) => stringOrItem(data)(_.id) },
      separator = Seq[Int](13).toJSArray,
      values = initialValuesJs,
      required = required
    ))
  
    // TODO: Note that Manifest actually tells us what's been added or removed, so in principle it's actually
    // pretty easy for us to generate a *change* event here, not necessarily a full rewrite! We are currently
    // grabbing and sending the full values list, but we could instead combine changeType and data into a
    // proper change event.
    $(element).on("manifestchange", { (/*evt:JQueryEventObject, changeType:String, data:js.Any*/) =>
      saveChange(values)
    })
  }
  
  def doRender() =
    input(cls:="_textEdit", tpe:="text")
}