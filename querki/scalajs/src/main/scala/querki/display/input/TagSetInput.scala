package querki.display.input

import scala.scalajs.js
import js.JSConverters._

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import org.querki.facades.manifest._

import querki.globals._

import querki.comm._

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
    
  def stringOrItem(data:js.Any)(f:ManifestItem => String):String = {
    if (data.isInstanceOf[js.prim.String]) 
      data.asInstanceOf[String]
    else 
      f(data.asInstanceOf[ManifestItem])
  }
  
  def propId:String 
  def required:Boolean
  def kind:TagSetKind.TagSetKind
  
  def marcoPoloDef = MarcoPoloOptions.
    url(controllers.ClientController.marcoPolo.spaceUrl(propId)).
    minChars(1).
    required(required).
    formatData({ (jq:JQuery, data:js.Array[js.Object]) => data }).
    formatItem({ data:js.Object => data.asInstanceOf[ManifestItem].display }:Function1[js.Object, js.Any]).
    formatNoResults({ (q:String) => s"No existing $kind with <b>$q</b> found." }:Function1[String, js.Any])
}
  
class TagSetInput(val propId:String, val required:Boolean, val kind:TagSetKind.TagSetKind, initialValuesJs:js.Dynamic)(implicit e:Ecology) 
  extends InputGadget[dom.HTMLInputElement](e) with MarcoPoloUser
{
  def values = { 
    $(elem).manifest(ManifestCommand.values).asInstanceOf[js.Array[String]].toList
  }
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    // The constructor for the Manifest object itself. This prompts you when you start typing,
    // using MarcoPolo, and organizes results into a nice list.
    $(elem).manifest(
      ManifestOptions.
        marcoPolo(marcoPoloDef).
        separator(Seq[Int](13).toJSArray).
        values(initialValuesJs).
        required(required).
        // TODO: this is fugly. Try simplifying after 0.6, and if that hasn't helped, think about this carefully:
        formatDisplay({ (data:js.Any) => stringOrItem(data.asInstanceOf[ManifestItem])(_.display).asInstanceOf[js.Any] }).
        formatValue({ (data:js.Any) => stringOrItem(data.asInstanceOf[ManifestItem])(_.id) })
    )
  
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


class MarcoPoloInput(val propId:String, val required:Boolean, val kind:TagSetKind.TagSetKind, mods:Modifier*)(implicit e:Ecology) 
  extends InputGadget[dom.HTMLInputElement](e) with MarcoPoloUser
{
  // We need to do stuff at Select time, and the external "marcopoloselect" event doesn't seem to work well enough:
  def marcoPoloDefWithSelect = {
    marcoPoloDef.
      onSelect(
        { (dataRaw:js.Any) => stringOrItem(dataRaw) { data =>
          val q = data.display
          onSelect(data)
          $(elem).value(q)
          q
        }}
      )
  }
  
  def values = List($(elem).value().asInstanceOf[String])

  /**
   * Usually, we save the value of this field. But this is broken out so that we can do something else
   * if this is being subclassed for a special purpose.
   */
  def onChange():Unit = {
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
