package models

object NavSection {
  object homeNav extends NavSections(Seq(querkiSection))
  
  def spaceNav(state:SpaceState) =
    NavSections(Seq(
      NavSection("This Space", Seq(
        NavLink("All Things", "things")
      )),
      querkiSection
    ))
  
  val querkiSection = NavSection("Querki", Seq(
      NavLink("Home", "/"),
      NavLink("Logout", "/logout")
      ))
}

case class NavSections(sections:Seq[NavSection])

case class NavSection(val title:String, val links:Seq[NavLink])

case class NavLink(display:String, url:String)
