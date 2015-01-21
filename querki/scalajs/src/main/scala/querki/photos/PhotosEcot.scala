package querki.photos

import querki.globals._

class PhotosEcot(e:Ecology) extends ClientEcot(e) {

  def implements = Set.empty
  
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  override def postInit() = {
//    Gadgets.registerSimpleGadget("._photoThumbnail", { new Thumbnail })
  }
}
