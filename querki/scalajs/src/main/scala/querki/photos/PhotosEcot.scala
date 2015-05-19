package querki.photos

import org.querki.jquery._

import querki.globals._

private [photos] trait PhotosInternal extends EcologyInterface {
  def recordTarget(target:PhotoTarget):Unit
  def findTargetFor(thumbnail:Thumbnail):Option[PhotoTarget]
  def showInDialog(thumbnail:Thumbnail):Unit
}

class PhotosEcot(e:Ecology) extends ClientEcot(e) with PhotosInternal {

  def implements = Set(classOf[PhotosInternal])
  
  lazy val Gadgets = interface[querki.display.Gadgets]  
  lazy val Pages = interface[querki.pages.Pages]
  
  override def postInit() = {
    Gadgets.registerSimpleGadget("._photoThumbnail", { new Thumbnail })
    Gadgets.registerSimpleGadget("._photoTarget", { new PhotoTarget })
    Gadgets.registerSimpleGadget("._photoEdit", { new PhotoInputButton })
    Gadgets.registerHook("._photoList")(PhotoList.hook)
  }
  
  val targetKey = "Photo Targets"
  type TargetMap = Map[String, PhotoTarget]
  
  val showDialogKey = "Show Photo Dialog"
  val showInputKey = "Show Photo Input"
  
  def recordTarget(target:PhotoTarget):Unit = {
    val page = Pages.findPageFor(target)
    val propId = target.fromProp
    // Note that this only works because we know the system is single-threaded!
    // Otherwise, we would need to use getOrElseUpdate.
    val newMap = page.getMetadata(targetKey).map(_.asInstanceOf[TargetMap]) match {
      case Some(map) => map + (propId -> target)
      case None => Map(propId -> target)
    }
    page.storeMetadata(targetKey, newMap)
  }
  
  def findTargetFor(thumbnail:Thumbnail):Option[PhotoTarget] = {
    val page = Pages.findPageFor(thumbnail)
    val prop = thumbnail.fromProp
    for {
      mapRaw <- page.getMetadata(targetKey)
      map = mapRaw.asInstanceOf[TargetMap]
      target <- map.get(prop)
    }
      yield target
  }
  
  def showInDialog(thumbnail:Thumbnail):Unit = {
    val page = Pages.findPageFor(thumbnail)
    // Since it isn't often needed, we build the ViewPhotoDialog on-demand
    val dialog = page.getMetadata(showDialogKey).map(_.asInstanceOf[ViewPhotoDialog]) match {
      case Some(dialog) => dialog
      case None => {
        val d = new ViewPhotoDialog
        $(page.elem).append(d.render)
        page.storeMetadata(showDialogKey, d)
        d
      }
    }
    dialog.showFrom(thumbnail)
  }
}
