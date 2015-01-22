package querki.photos

import querki.globals._

private [photos] trait PhotosInternal extends EcologyInterface {
  def recordTarget(target:PhotoTarget):Unit
  def findTargetFor(thumbnail:Thumbnail):Option[PhotoTarget]
}

class PhotosEcot(e:Ecology) extends ClientEcot(e) with PhotosInternal {

  def implements = Set(classOf[PhotosInternal])
  
  lazy val Gadgets = interface[querki.display.Gadgets]  
  lazy val Pages = interface[querki.pages.Pages]
  
  override def postInit() = {
    Gadgets.registerSimpleGadget("._photoThumbnail", { new Thumbnail })
    Gadgets.registerSimpleGadget("._photoTarget", { new PhotoTarget })
  }
  
  val targetKey = "Photo Targets"
  type TargetMap = Map[String, PhotoTarget]
  
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
}
