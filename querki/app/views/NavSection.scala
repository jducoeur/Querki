package views

object TestNavSection extends NavSection("Testing nav sections", Seq(
    NavLink("My Homepage", "http://jducoeur.org/"),
    NavLink("Look! Google!", "http://google.com/"),
    NavLink("Querki Home", "/")
    ))

class NavSection(val title:String, val links:Seq[NavLink])

case class NavLink(display:String, url:String)