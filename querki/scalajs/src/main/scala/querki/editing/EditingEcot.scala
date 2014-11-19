package querki.editing

import querki.globals._

class EditingEcot(e:Ecology) extends ClientEcot(e) with Editing {

  def implements = Set(classOf[Editing])
  
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val editInstancesFactory = Pages.registerStandardFactory("_editInstances", { (params) => new EditInstancesPage(params) })
  
  override def postInit() = {
    editInstancesFactory
  }
}
