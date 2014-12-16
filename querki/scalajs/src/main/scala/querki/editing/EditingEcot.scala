package querki.editing

import querki.globals._

class EditingEcot(e:Ecology) extends ClientEcot(e) with Editing {

  def implements = Set(classOf[Editing])
  
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val editInstancesFactory = Pages.registerStandardFactory("_editInstances", { (params) => new EditInstancesPage(params) })
  lazy val modelDesignerFactory = Pages.registerStandardFactory("_modelDesigner", { (params) => new ModelDesignerPage(params) })
  
  override def postInit() = {
    editInstancesFactory
    modelDesignerFactory
  }
  
  def showAdvancedEditorFor(thingId:String) = {
    PageManager.showPage("_modelDesigner", Map("modelId" -> thingId))
  }
  
  def propPath(propId:String, thingIdOpt:Option[String]):String = {
    val thingId = thingIdOpt.getOrElse("")
    // TODO: this is evil magic knowledge that just happens to match FieldIds on the server. We need
    // a better shared mechanism here:
    // TODO: for that matter, this format is antiquated and should be changed -- at the least, the v- prefix
    // is unnecessary:
    s"v-$propId-$thingId"
  }
  def propPath(propId:String):String = propPath(propId, None)
}
