package querki.editing

import scala.concurrent.Future

import autowire._

import querki.display.RawDiv
import querki.globals._

import EditFunctions._

class EditingEcot(e:Ecology) extends ClientEcot(e) with Editing {

  def implements = Set(classOf[Editing])
  
  lazy val Client = interface[querki.client.Client]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val editInstancesFactory = Pages.registerThingPageFactory("_editInstances", { (params) => new EditInstancesPage(params) }, "modelId")
  lazy val modelDesignerFactory = Pages.registerThingPageFactory("_modelDesigner", { (params) => new ModelDesignerPage(params) }, "modelId")
  lazy val advancedEditorFactory = Pages.registerThingPageFactory("_advancedEditor", { (params) => new ModelDesignerPage(params) }, "thingId")
  lazy val editSpaceInfoFactory = Pages.registerStandardFactory("_editSpaceInfo", { (params) => new EditSpaceInfoPage(params) })
  
  override def postInit() = {
    editInstancesFactory
    modelDesignerFactory
    advancedEditorFactory
    editSpaceInfoFactory
    Gadgets.registerSimpleGadget("._advancedEditButton", { new AdvancedEditButton })
    Gadgets.registerSimpleGadget("._pickList", { new PickListGadget })
    Gadgets.registerSimpleGadget("._checklist", { new CheckList })
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
  
  def getSomePropertyEditors(thingId:TID, propIds:TID*):Future[Map[TID, Gadget[_]]] = {
    val futs = propIds.map { propId =>
      Client[EditFunctions].getOnePropertyEditor(thingId, propId).call()
    }
    Future.sequence(futs).map(infos => infos.map { info => (info.propInfo.oid, new RawDiv(info.editor)) }).map(Map(_:_*))
  }
}
