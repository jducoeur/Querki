package querki.editing

import querki.globals._

class EditingEcot(e:Ecology) extends ClientEcot(e) with Editing {

  def implements = Set(classOf[Editing])
  
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val editInstancesFactory = Pages.registerStandardFactory("_editInstances", { (params) => new EditInstancesPage(params) })
  lazy val modelDesignerFactory = Pages.registerStandardFactory("_modelDesigner", { (params) => new ModelDesignerPage(params) })
  
  override def postInit() = {
    editInstancesFactory
    modelDesignerFactory
  }
}
