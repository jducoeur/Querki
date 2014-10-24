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

object TagSetKind {
  type TagSetKind = String
  
  val Tag = "tag"
  val Link = "thing"
}

trait MarcoPoloUser extends EcologyMember {
  lazy val controllers = interface[querki.comm.ApiComm].controllers
    
  def stringOrItem(data:js.Any)(f:ManifestItem => String) = {
    if (data.isInstanceOf[js.prim.String]) 
      data 
    else 
      f(data.asInstanceOf[ManifestItem])
  }
  
  def propId:String 
  def required:Boolean
  def kind:TagSetKind.TagSetKind
  
  def marcoPoloDef = lit(
    url = controllers.ClientController.marcoPolo.spaceUrl(propId),
    minChars = 1,
    required = required,
    formatData = { data:js.Object => data },
    formatItem = { (data:ManifestItem) => data.display },
    formatNoResults = { (q:String) => s"No existing $kind with <b>$q</b> found." }
  )
}
  
class TagSetInput(val propId:String, val required:Boolean, val kind:TagSetKind.TagSetKind, initialValuesJs:js.Dynamic)(implicit e:Ecology) 
  extends InputGadget[dom.HTMLInputElement](e) with MarcoPoloUser
{
  def values = { 
    $(elem).manifest("values").asInstanceOf[js.Array[String]].toList
  }
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    // The constructor for the Manifest object itself. This prompts you when you start typing,
    // using MarcoPolo, and organizes results into a nice list.
    $(elem).manifest(lit(
      marcoPolo = marcoPoloDef,
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
    $(elem).on("manifestchange", { (/*evt:JQueryEventObject, changeType:String, data:js.Any*/) =>
      save()
    })
  }
  
  def doRender() =
    input(cls:="_textEdit", tpe:="text")
}

object TagSetInput {
  /**
   * Create a TagSetInput from an existing DOM Element. The Element is required to have several data
   * attributes set, giving the critical details.
   */
  def apply(rawElement:dom.Element)(implicit e:Ecology) = {
    // Required data attributes of any Tag Set Input:
    val isNames = $(rawElement).data("isnames").asInstanceOf[Boolean]
    val initialValuesJs = $(rawElement).data("current")
    val propId = $(rawElement).data("prop").asInstanceOf[String]
    val kind = if (isNames) TagSetKind.Tag else TagSetKind.Link
    
    new TagSetInput(propId, !isNames, kind, initialValuesJs).setElem(rawElement)
  }
}


trait MarcoPoloFacade extends JQuery {
  def marcoPolo(config:js.Object):this.type = ???
}
object MarcoPoloFacade {
  implicit def jqMarcoPolo(jq:JQuery):MarcoPoloFacade = jq.asInstanceOf[MarcoPoloFacade]
}
import MarcoPoloFacade._

class MarcoPoloInput(val propId:String, val required:Boolean, val kind:TagSetKind.TagSetKind, mods:Modifier*)(implicit e:Ecology) 
  extends InputGadget[dom.HTMLInputElement](e) with MarcoPoloUser
{
  // We need to do stuff at Select time, and the external "marcopoloselect" event doesn't seem to work well enough:
  def marcoPoloDefWithSelect:js.Object = {
    val mpd = marcoPoloDef
    mpd.onSelect = 
      { (dataRaw:js.Any) => stringOrItem(dataRaw) { data =>
        val q = data.display
        onSelect(data)
        $(elem).value(q)
        q
      }}
    mpd
  }
  
  def values = List($(elem).value().asInstanceOf[String])

  /**
   * Usually, we save the value of this field. But this is broken out so that we can do something else
   * if this is being subclassed for a special purpose.
   */
  def onChange() = {
    save()
  }
  
  /**
   * For subclasses to hook.
   */
  def onSelect(item:ManifestItem) = {}
  
  def hook() = {
    $(elem).marcoPolo(marcoPoloDefWithSelect)
    
    $(elem).change( { (evt:JQueryEventObject) =>
      onChange()
    })
  }
  
  def doRender() =
    input(cls:="_tagInput", tpe:="text", mods)
}

object MarcoPoloInput {
  /**
   * Create a TagSetInput from an existing DOM Element. The Element is required to have several data
   * attributes set, giving the critical details.
   */
  def apply(rawElement:dom.Element)(implicit e:Ecology) = {
    // Required data attributes of any Tag Set Input:
    val isNames = $(rawElement).data("isnames").asInstanceOf[Boolean]
    val propId = $(rawElement).data("prop").asInstanceOf[String]
    val kind = if (isNames) TagSetKind.Tag else TagSetKind.Link
    
    new MarcoPoloInput(propId, !isNames, kind).setElem(rawElement)
  }
}
