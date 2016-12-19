package querki.photos

import scala.scalajs.js
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

import org.querki.facades.bootstrap._

class ViewPhotoDialog(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] {

  def showFrom(thumbnail:Thumbnail) = {
    // TODO: yes, there's a lot of horrible hardcoded magic here. Can we use CSS to make this less awful?
    $(elem).find(".modal-dialog")
      .width(thumbnail.fullWidth + 31)
      .height(thumbnail.fullHeight + 50)
      // TBD: can we use Scalatags here somehow?
      .css(js.Dictionary[js.Any](
        "max-height" -> "100%"
      ))  
      
    $(photoImage.elem).attr("src", thumbnail.fullSrc).width(thumbnail.fullWidth).height(thumbnail.fullHeight)
    $(elem).modal(ModalCommand.show)
  }
  
  lazy val photoImage = Gadget(img())

  def doRender() =
    div(cls:="modal",
      tabindex:="-1",
      role:="dialog",
      aria.labelledby:="View Photo",
      aria.hidden:="true",
      title:="View Photo",
      display:="none",
      div(
        cls:="modal-dialog",
        div(
          cls:="modal-content",
          div(id:="photo-full-header", cls:="modal-header",
            button(
              tpe:="button", 
              cls:="close", 
              data("dismiss"):="modal",
              aria.label:="Close",
              span(aria.hidden:="true", raw("&times;"))
            ),
            p(cls:="modal-title", b("View Image"))
          ),
          div(cls:="modal-body",
            maxHeight:="100%",
            photoImage
          )
        )
      )
    )
}