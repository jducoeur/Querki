package querki.display

import org.scalajs.dom

import scalatags.JsDom.all._

import querki.globals._

class PageManagerEcot(e:Ecology) extends ClientEcot(e) with PageManager {
  def implements = Set(classOf[PageManager])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  var _displayRoot:Option[dom.Element] = None
  def displayRoot = _displayRoot.get
  
  /**
   * Declare the top-level container that we are going to render the page into. This
   * is typically going to be the body itself, but we're deliberately not assuming that.
   */
  @JSExport
  def setRoot(root:dom.Element) = {
    _displayRoot = Some(root)
  }
  
  var _imagePath:Option[String] = None
  def imagePath = _imagePath.get
  @JSExport
  def setImagePath(path:String) = {
    _imagePath = Some(path)
  }
  
  /**
   * Utility, to make 
   */
  def data(name:String):Attr = scalatags.generic.Attr(s"data-$name")
  
  /**
   * Actually display the full page.
   */
  @JSExport
  def renderPage() = {
    val menuBar =
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
                img(src:=s"$imagePath/Logo-menubar.png")
              )
            )
          )
        )
      )
      
    val guts =
      div(cls:="guts container-fluid",
        div(cls:="row-fluid",
          div(cls:="querki-content span12",
            
            h3(s"User ${UserAccess.name}"),
            p(
              (DataAccess.mainThing match {
                case Some(thing) => s"We are showing ${thing.displayName} (${thing.oid}, kind: ${thing.kind})"
                case None => "We don't have a Thing!"
              }) + (DataAccess.space match {
                case Some(space) => s" in Space ${space.displayName} (${space.oid})"
                case None => "No space!"
              })
            )
            
          )
        )
      )
      
    val page =
      div(menuBar, guts)
    
    $(displayRoot).empty()
    $(displayRoot).append(page.render)
  }
}
