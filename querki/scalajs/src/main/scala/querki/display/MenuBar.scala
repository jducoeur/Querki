package querki.display

import org.scalajs.dom

import scalatags.JsDom.all._

import querki.globals._

class MenuBar(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {
  
  lazy val PageManager = interface[PageManager]
  
  def doRender() =
      div(cls:="container",
        div(cls:="navbar navbar-fixed-top _noPrint",
          div(cls:="navbar-inner",
            div(cls:="container",
              
              // This is the collapsed menu icon that we show on a small screen:
              a(cls:="btn btn-navbar",
                data("toggle"):="collapse",
                data("target"):=".nav-collapse",
                span(cls:="icon-bar"),
                span(cls:="icon-bar"),
                span(cls:="icon-bar")
              ),
              
              // Show the logo on the left-hand side:
              a(cls:="brand",
                // TODO: where should we define this call?
                href:="/",
                img(src:=s"${PageManager.imagePath}/Logo-menubar.png")
              )
            )
          )
        )
      )
}
