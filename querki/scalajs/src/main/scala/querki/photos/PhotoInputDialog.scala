package querki.photos

import java.util.regex.Pattern

import scala.scalajs.js
import js.annotation.JSName
import org.scalajs.dom
import org.querki.jquery._

import scalatags.JsDom.all._
import upickle.default._

import org.querki.facades.bootstrap._
import org.querki.facades.fileupload._

import models.Wikitext

import querki.globals._

import querki.comm._
import querki.display.{HookedGadget, RawSpan}
import querki.pages.Page

// Necessary DOM enhancement -- the scala-js-dom doesn't realize that EventTarget can have
// a "files" member, but it does in the case of a file input field
// TODO: submit this as a PR to scala-js-dom:
@js.native
trait FileTarget extends js.Object {
  def files:dom.raw.FileList = js.native
}
object FileTarget {
  implicit def EventTarget2FileTarget(t:dom.EventTarget):FileTarget = t.asInstanceOf[FileTarget]
}
import FileTarget._

/**
 * This represents a button labeled something like "Add Photo", with metadata about where to
 * put the resulting photo.
 */
class PhotoInputButton(implicit e:Ecology) extends HookedGadget[dom.html.Input](e) {
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val PhotosInternal = interface[PhotosInternal]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  // At least for now, this just wraps incoming buttons:
  def doRender() = ???
  
  lazy val propId = $(elem).parent().data("propid").asInstanceOf[String]
  lazy val collId = $(elem).parent().data("collid").asInstanceOf[String]
  lazy val thing = $(elem).parent().data("thing").asInstanceOf[String]
  
  lazy val isSingleton:Boolean = {
    val core = DataAccess.std.core
    (collId == core.exactlyOneColl.oid.underlying) || (collId == core.optionalColl.oid.underlying)
  }
  
  def hasExistingPhotos:Boolean = {
    $(elem).parent().find("._photoThumbnail").length > 0
  }
  
  lazy val addReadyIcon = Gadget(i(cls:="glyphicon glyphicon-camera _addPhotoIcon"))
  lazy val addRunningIcon = Gadget(i(cls:="fa fa-spinner fa-3x fa-pulse _addPhotoIcon"))
  
  def showState(indicator:Gadget[dom.html.Element]) = {
    val frame = $(elem)
    frame.find("._addPhotoIcon").detach()
    val indicatorElem = indicator.rendered
    frame.append(indicatorElem)
  }
  def showReady() = {
    showState(addReadyIcon)
    $(elem).data("ready", true)
  }
  def showRunning() = {
    showState(addRunningIcon)
    $(elem).data("ready", false)
  }
  
  def addRealInputButton():Unit = {
    val photoInputElem = Gadget(input(cls:="_photoInputElem", tpe:="file", accept:="image/*;capture=camera"))
    
    def clickTransfer(evt:JQueryEventObject) = {
      $(elem).data("ready").asInstanceOf[js.UndefOr[Boolean]].map { ready =>
        if (ready)
          $(photoInputElem.elem).click()        
      }
    }
    val clickT:js.Function1[JQueryEventObject, Any] = clickTransfer _
     
    $(elem).on("click", clickT)
    
    $(elem).after(photoInputElem.rendered)
    val agent = dom.window.navigator.userAgent
    val disableResize = (agent.contains("Opera") || (agent.contains("Android") && !agent.contains("Chrome")))
    $(photoInputElem.elem).fileupload(FileUploadOptions
      .url(
        controllers.PhotoController.upload(
          DataAccess.userName, 
          DataAccess.spaceId.underlying, 
          // TODO: change the signature of upload() to take propId as a proper parameter.
          // Not sure why I did it this awkward way in the old code, but it's now just stupid:
          thing).url + "&propId=" + propId)
      .multipart(false)
      .maxFileSize(5000000) // 5 MB
      // Enable image resizing, except for Android and Opera,
      // which actually support image resizing, but fail to
      // send Blob objects via XHR requests:
      .disableImageResize(disableResize)
      // Cap it at 1000 on a side, which is the current Querki maximum. Note that the server
      // may reduce this further based on user preference, but we leave it to do further
      // reduction there, since the server's algorithm does it more cleanly, with less aliasing.
      // Here, we are reducing it mainly to reduce unnecessary transmission time. 
      .imageMaxWidth(1000)
      .imageMaxHeight(1000)
      .processstart({ evt:JQueryEventObject =>
        showRunning()
      })
      .progress({ (evt:JQueryEventObject, data:FileUploadProgress) =>
        val percent = (data.loaded / data.total) * 100
        // TODO: is this ever producing useful information?
        println(s"Progress: $percent")
      })
      .done({ (evt:JQueryEventObject, data:FileUploadResults) =>
        if (isSingleton && hasExistingPhotos) {
          // Need to delete the old thumbnail first
          $(elem).parent().find("._photoThumbnailFrame").has("._photoThumbnail").remove()
        }
        
        // The result should be the pickled Wikitext of the new thumbnail:
        val wiki = read[Wikitext](data.result.asInstanceOf[String])
        val wikiStr = wiki.raw.html.toString
        val rawGadget = new RawSpan(wikiStr)
        rawGadget.render
        $(rawGadget.elem).insertBefore(elem)
        Gadgets.hookPendingGadgets()
        
	    // TODO: for the moment, we are using the real input button as a one-shot -- after each upload, we're removing
	    // it and creating another. This is stupid, but it is working around the fact that things only seem to be
	    // working once otherwise. Figure out why that's happening!
        $(elem).off("click")
        $(photoInputElem.elem).remove()
        addRealInputButton()
        
        showReady()
      })
    )
    
    $(photoInputElem.elem).change({ evt:JQueryEventObject =>
      val file = evt.target.files(0)
      if (Pattern.matches("image.*", file.`type`)) {
        val reader = new dom.FileReader()
        reader.onload = { uievt:dom.UIEvent => }
      } else {
        StatusLine.showBriefly(s"That is a ${file.`type`}. You can only upload images.")
      }
    })

  }
  
  def hook() = {
    // This is a tad stupid, but necessary to get the visual layout of the thumbnails to work
    // right. See http://stackoverflow.com/questions/7273338/how-to-vertically-align-an-image-inside-div
    val verticalHelper = Gadget(span(cls:="_photoThumbnailHelper"))
    $(elem).append(verticalHelper.rendered)
    
    addRealInputButton()
 
    val text =
      if (isSingleton && hasExistingPhotos)
        "Click to replace the photo"
      else
        "Click to add a photo"
	$(elem).tooltip(TooltipOptions.title(text))
	
    showReady()
  }
} 
