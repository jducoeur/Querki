package querki.apps

import querki.ecology._
import querki.globals._

/**
 * @author jducoeur
 */
class AppsEcot(e:Ecology) extends ClientEcot(e) with Apps {
  def implements = Set(classOf[Apps])
  
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val appMgmtFactory = Pages.registerStandardFactory("_appMgmt", { (params) => new AppManagementPage(params) })
  lazy val extractAppFactory = Pages.registerStandardFactory("_extractApp", { (params) => new ExtractAppPage(params) })
  
  override def postInit() = {
    appMgmtFactory
    extractAppFactory
  }
}
