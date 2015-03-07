package querki.photos

import org.scalajs.dom
import scalatags.JsDom.all._

import querki.globals._

import org.querki.facades.bootstrap._

import querki.display.Gadget

class ViewPhotoDialog extends Gadget[dom.HTMLDivElement] {

  def showFrom(thumbnail:Thumbnail) = {
    // TODO: yes, there's a lot of horrible hardcoded magic here. Can we use CSS to make this less awful?
    $(elem)
      .width(thumbnail.fullWidth + 31)
      .height(thumbnail.fullHeight + 50)
      // TBD: can we use Scalatags here somehow?
      .css(lit(
        "max-height" -> "100%",
        "margin-left" -> s"-${thumbnail.fullWidth / 2}px"
      ))
      
    $(photoImage.elem).attr("src", thumbnail.fullSrc).width(thumbnail.fullWidth).height(thumbnail.fullHeight)
    $(elem).modal(ModalCommand.show)
  }
  
  lazy val photoImage = Gadget(img())

  def doRender() =
    div(cls:="modal hide",
      title:="View Photo",
      display:="none",
      div(id:="photo-full-header", cls:="modal-header",
        // TODO: add aria.hidden:="true" -- Aria support is new in Scalatags:
        button(tpe:="button", cls:="close", data("dismiss"):="modal", "x"),
        p(b("View Image"))
      ),
      div(cls:="modal-body",
        maxHeight:="100%",
        photoImage
      )
    )
}