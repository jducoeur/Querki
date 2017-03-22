package querki.admin

import querki.globals._

class AdminEcot(e:Ecology) extends ClientEcot(e) with Admin {

  def implements = Set(classOf[Admin])
  
  lazy val Pages = interface[querki.pages.Pages]

  lazy val statisticsFactory = Pages.registerStandardFactory("_adminStats", { new StatisticsPage(_) })
  lazy val manageUsersFactory = Pages.registerStandardFactory("_manageUsers", { new ManageUsersPage(_) })
  lazy val monitorFactory = Pages.registerStandardFactory("_monitor", { new MonitorPage(_) })
  lazy val spacesTimingFactory = Pages.registerStandardFactory("_spacesTiming", { new SpacesTimingPage(_) })
  lazy val spaceTimingFactory = Pages.registerStandardFactory("_spaceTiming", { new SpaceTimingPage(_) })
  
  override def postInit() = {
    // Instantiate the Pages:
    statisticsFactory
    manageUsersFactory
    monitorFactory
    spacesTimingFactory
    spaceTimingFactory
  }
}
