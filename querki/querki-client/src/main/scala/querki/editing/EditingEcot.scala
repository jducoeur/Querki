package querki.editing

import querki.globals._

class EditingEcot(e:Ecology) extends ClientEcot(e) with Editing {

  def implements = Set(classOf[Editing])
  
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val editInstancesFactory = Pages.registerThingPageFactory("_editInstances", { (params) => new EditInstancesPage(params) }, "modelId")
  lazy val modelDesignerFactory = Pages.registerThingPageFactory("_modelDesigner", { (params) => new ModelDesignerPage(params) }, "modelId")
  lazy val advancedEditorFactory = Pages.registerThingPageFactory("_advancedEditor", { (params) => new ModelDesignerPage(params) }, "thingId")
  
  override def postInit() = {
    editInstancesFactory
    modelDesignerFactory
    advancedEditorFactory
    Gadgets.registerSimpleGadget("._advancedEditButton", { new AdvancedEditButton })
    Gadgets.registerSimpleGadget("._pickList", { new PickListGadget })
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
  
  def propPathOldStyleHack(propId:TID, thingIdOpt:Option[TID]):String = {
    val actualPropId = TID(propId.underlying.substring(1))
    val actualThingIdOpt = thingIdOpt.map(tid => TID(tid.underlying.substring(1)))
    propPath(actualPropId, actualThingIdOpt)
  }
}
