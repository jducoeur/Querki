package querki.display

import org.scalajs.dom

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
  
  /**
   * Actually display the full page.
   */
  @JSExport
  def renderPage() = {
    val contents = (DataAccess.mainThing match {
      case Some(thing) => s"We are showing ${thing.displayName} (${thing.oid})"
      case None => "We don't have a Thing!"
    }) + (DataAccess.space match {
      case Some(space) => s" in Space ${space.displayName} (${space.oid})"
      case None => "No space!"
    })
    
    $(displayRoot).html(s"""<h3>User ${UserAccess.name}</h3>
        |<p>$contents</p>""".stripMargin)
  }
}
