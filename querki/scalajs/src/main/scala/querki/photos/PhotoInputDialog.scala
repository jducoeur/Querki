package querki.photos

import java.util.regex.Pattern

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import org.querki.facades.bootstrap._
import org.querki.facades.fileupload._

import querki.globals._

import querki.comm._
import querki.display.Gadget
import querki.display.input.InputGadget

/**
 * This represents a button labeled something like "Add Photo", with metadata about where to
 * put the resulting photo.
 */
class PhotoInputButton(implicit e:Ecology) extends InputGadget[dom.HTMLInputElement](e) {
  
  lazy val PhotosInternal = interface[PhotosInternal]
  
  // At least for now, this just wraps incoming buttons:
  def doRender() = ???
  def values = ???
  
  lazy val propId = $(elem).data("propid").asInstanceOf[String]
  lazy val thing = $(elem).data("thing").asInstanceOf[String]
  
  def hook() = {
    $(elem).click({ evt:JQueryEventObject =>
      PhotosInternal.showInputDialog(this)
    })
  }
} 

// Necessary DOM enhancement -- the scala-js-dom doesn't realize that EventTarget can have
// a "files" member, but it does in the case of a file input field
// TODO: submit this as a PR to scala-js-dom:
trait FileTarget extends js.Object {
  def files:dom.FileList = ???
}
object FileTarget {
  implicit def EventTarget2FileTarget(t:dom.EventTarget):FileTarget = t.asInstanceOf[FileTarget]
}
import FileTarget._

class PhotoInputDialog(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[querki.display.PageManager]
  
  def showFrom(button:PhotoInputButton) = {
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
      .disableImageResize(Pattern.matches("Android(?!.*Chrome)|Opera", dom.window.navigator.userAgent))
      // Cap it at 1000 on a side, which is the current Querki maximum. Note that the server
      // may reduce this further based on user preference, but we leave it to do further
      // reduction there, since the server's algorithm does it more cleanly, with less aliasing.
      // Here, we are reducing it mainly to reduce unnecessary transmission time. 
      .imageMaxWidth(1000)
      .imageMaxHeight(1000)
      .start({ evt:JQueryEventObject => setStatus("Uploading...")})
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
        // TODO: for now, we're just hard-reloading the page to refresh the images. This is crude:
        // we should build a smarter protocol, and just refresh the relevant images.
        PageManager.reload()
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
  
  lazy val photoInputElem = Gadget(input(tpe:="file", accept:="image/*;capture=camera"))
  lazy val photoStatus = Gadget(p(raw("&nbsp;")))
  lazy val photoProgressBar = Gadget(div(cls:="bar", width:="0%"))
  lazy val photoProgress = Gadget(div(cls:="progress progress-striped active", photoProgressBar))
  lazy val photoThumb = Gadget(span())

  def doRender() =
    // TODO: add aria-labelledby="myModalLabel" aria-hidden="true"
    div(cls:="modal hide",
      tabindex:="-1",
      role:="dialog",
      display:="none",
      div(cls:="modal-header",
        button(tpe:="button", cls:="close", data("dismiss"):="modal", "x"),
        h3("Take a photo")
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
}
