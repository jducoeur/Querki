package querki.photos

import java.util.regex.Pattern

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._

import scalatags.JsDom.all._
import upickle._

import org.querki.facades.bootstrap._
import org.querki.facades.bootstrap.filestyle._
import org.querki.facades.fileupload._

import models.Wikitext

import querki.globals._

import querki.comm._
import querki.display.{Gadget, RawSpan}
import querki.display.input.InputGadget
import querki.pages.Page

// Necessary DOM enhancement -- the scala-js-dom doesn't realize that EventTarget can have
// a "files" member, but it does in the case of a file input field
// TODO: submit this as a PR to scala-js-dom:
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
class PhotoInputButton(implicit e:Ecology) extends InputGadget[dom.html.Input](e) {
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val PhotosInternal = interface[PhotosInternal]
  
  // At least for now, this just wraps incoming buttons:
  def doRender() = ???
  def values = ???
  
  lazy val propId = $(elem).parent().data("propid").asInstanceOf[String]
  lazy val thing = $(elem).parent().data("thing").asInstanceOf[String]
  
  lazy val photoInputElem = Gadget(input(cls:="_photoInputElem", tpe:="file", accept:="image/*;capture=camera"))
  lazy val addReadyIcon = Gadget(i(cls:="glyphicon glyphicon-plus _addPhotoIcon"))
  lazy val addRunningIcon = Gadget(i(cls:="fa fa-spinner fa-3x fa-pulse _addPhotoIcon"))
  
  def showState(indicator:Gadget[dom.Element]) = {
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
  
  def hook() = {
    photoInputElem.render
    $(elem).after(photoInputElem.elem)
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
//        println(s"Progress: $percent")
//        $(photoProgressBar.elem).css("width", s"$percent%")
//        if (data.loaded == data.total)
//          setStatus("Processing...")
      })
      .done({ (evt:JQueryEventObject, data:FileUploadResults) =>
        val wiki = upickle.read[Wikitext](data.result.asInstanceOf[String])
        val wikiStr = wiki.raw.html.toString
        val rawGadget = new RawSpan(wikiStr)
        rawGadget.render
        $(rawGadget.elem).insertBefore(elem)
        InputGadgets.hookPendingGadgets()
        
        showReady()
      })
    )
    
    $(photoInputElem.elem).change({ evt:JQueryEventObject =>
      val file = evt.target.files(0)
      if (Pattern.matches("image.*", file.`type`)) {
        val reader = new dom.FileReader()
        reader.onload = { uievt:dom.UIEvent =>
//          // Render the thumbnail
//          val thumb = img(height:=120, src:=reader.result, title:=file.name)
//          $(photoThumb.elem).append(thumb.render)
        }
      } else {
//        setStatus(s"I don't know what that is -- the type is ${file.`type`}.")
      }
    })
    
	$(elem).tooltip(TooltipOptions.title("Click to add a new photo"))
	
    $(elem).click({ evt:JQueryEventObject =>
//      PhotosInternal.showInputDialog(this)
      $(elem).data("ready").asInstanceOf[js.UndefOr[Boolean]].map { ready =>
        if (ready)
          $(photoInputElem.elem).click()        
      }
    })
    
    showReady()
  }
} 

class PhotoInputDialog(page:Page)(implicit val ecology:Ecology) extends Gadget[dom.html.Div] with EcologyMember {
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  lazy val PageManager = interface[querki.display.PageManager]
  
  def showFrom(button:PhotoInputButton) = {
    InputGadgets.hookPendingGadgets()
    val agent = dom.window.navigator.userAgent
    val disableResize = (agent.contains("Opera") || (agent.contains("Android") && !agent.contains("Chrome")))
    $(photoInputElem.elem).fileupload(FileUploadOptions
      .url(
        controllers.PhotoController.upload(
          DataAccess.userName, 
          DataAccess.spaceId.underlying, 
          // TODO: change the signature of upload() to take propId as a proper parameter.
          // Not sure why I did it this awkward way in the old code, but it's now just stupid:
          button.thing).url + "&propId=" + button.propId)
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
      .start({ evt:JQueryEventObject =>
        setStatus("Uploading...")
      })
      .progress({ (evt:JQueryEventObject, data:FileUploadProgress) =>
        val percent = (data.loaded / data.total) * 100
        println(s"Progress: $percent")
        $(photoProgressBar.elem).css("width", s"$percent%")
        if (data.loaded == data.total)
          setStatus("Processing...")
      })
      .done({ (evt:JQueryEventObject, data:FileUploadResults) =>
        $(photoProgress.elem).removeClass("active")
        setStatus("Done!")
        println(data.result)
        $(elem).modal(ModalCommand.hide)
        // Refresh the Page, in case we're currently displaying this photo:
        page.refresh()
      })
    )
    
    $(photoInputElem.elem).change({ evt:JQueryEventObject =>
      val file = evt.target.files(0)
      if (Pattern.matches("image.*", file.`type`)) {
        val reader = new dom.FileReader()
        reader.onload = { uievt:dom.UIEvent =>
          // Render the thumbnail
          val thumb = img(height:=120, src:=reader.result, title:=file.name)
          $(photoThumb.elem).append(thumb.render)
        }
      } else {
        setStatus(s"I don't know what that is -- the type is ${file.`type`}.")
      }
    })
    
    $(elem).modal(ModalCommand.show)
  }
  
  def setStatus(msg:String) = $(photoStatus.elem).text(msg)
  
  class PhotoUploadButton(implicit e:Ecology) extends InputGadget[dom.html.Input](e) {
    def doRender() =
      input(cls:="_photoInputElem", tpe:="file", accept:="image/*;capture=camera")
  
    def values = ???
  
    def hook() = {
      $(elem).filestyle(BootstrapFilestyleOptions.
        input(false).
        iconName("glyphicon-camera").
        buttonText("Take / upload picture")
      )
    }
  }
  
  lazy val photoInputElem = new PhotoUploadButton
  lazy val photoStatus = Gadget(p(raw("&nbsp;")))
  lazy val photoProgressBar = Gadget(div(cls:="bar", width:="0%"))
  lazy val photoProgress = Gadget(div(cls:="progress progress-striped active", photoProgressBar))
  lazy val photoThumb = Gadget(span())

  def doRender() =
    div(cls:="modal fade",
      tabindex:="-1",
      role:="dialog",
      aria.labelledby:="Take a photo",
      aria.hidden:="true",
      div(cls:="modal-dialog",
        div(cls:="modal-content",
	      div(cls:="modal-header",
	        button(tpe:="button", cls:="close", data("dismiss"):="modal", "x"),
	        h3(cls:="modal-title", "Take a photo")
	      ),
	      div(cls:="modal-body",
	        p("""Press the button below, then choose "Camera" to take a photo now:"""),
	        p(photoInputElem),
	        photoStatus,
	        photoProgress,
	        photoThumb
	      ),
	      div(cls:="modal-footer",
	        button(cls:="button", data("dismiss"):="modal", "Cancel")
	      )
	    )
      )
    )
}
