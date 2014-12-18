package querki.editing

import querki.globals._

class EditingEcot(e:Ecology) extends ClientEcot(e) with Editing {

  def implements = Set(classOf[Editing])
  
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val editInstancesFactory = Pages.registerStandardFactory("_editInstances", { (params) => new EditInstancesPage(params) })
  lazy val modelDesignerFactory = Pages.registerStandardFactory("_modelDesigner", { (params) => new ModelDesignerPage(params) })
  lazy val advancedEditorFactory = Pages.registerStandardFactory("_advancedEditor", { (params) => new ModelDesignerPage(params) })
  
  override def postInit() = {
    editInstancesFactory
    modelDesignerFactory
    advancedEditorFactory
    Gadgets.registerSimpleGadget("._advancedEditButton", { new AdvancedEditButton })
  }
  
  def showAdvancedEditorFor(thingId:TID, asModel:Boolean = true) = {
    if (asModel)
      PageManager.showPage("_modelDesigner", Map("modelId" -> thingId.underlying))
    else
      PageManager.showPage("_advancedEditor", Map("thingId" -> thingId.underlying))
  }
  
  def propPath(propId:TID, thingIdOpt:Option[TID]):String = {
    val thingId = thingIdOpt.getOrElse(TID(""))
    // TODO: this is evil magic knowledge that just happens to match FieldIds on the server. We need
    // a better shared mechanism here:
    // TODO: for that matter, this format is antiquated and should be changed -- at the least, the v- prefix
    // is unnecessary:
    s"v-${propId.underlying}-${thingId.underlying}"
  }
  def propPath(propId:TID):String = propPath(propId, None)
}
