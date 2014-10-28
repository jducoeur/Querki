package querki.qtext

import scala.scalajs.js

import querki.globals._

class QTextUtilsEcot(e:Ecology) extends ClientEcot(e) with QTextUtils {
  
  def implements = Set(classOf[QTextUtils])
  
  /**
   * RegExp for old-fashioned paths into Querki Spaces.
   */
  val spacePathRegExp = js.RegExp("/u/([\\.\\w\\-]+)/([\\.\\w\\-]+)/([\\.\\w\\-]+)")
  
  def adjustUrl(urlIn:String):String = {
    if (spacePathRegExp.test(urlIn)) {
      // This is a server-generated URL that is pointing into user space. We may need to tweak it,
      // but first we need to deconstruct it.
      val matches = spacePathRegExp.exec(urlIn)
      val userId = matches(1)
      val spaceId = matches(2)
      val rest = matches(3)
      // TODO: detect if the userId is the OID of the current page, and change to ThingId if so.
      // Ditto for the spaceId. Try to change /u/.0d9djkd/.98dk398d/stuff to /u/Joe/Workroom/stuff,
      // if we are currently looking at Joe/Workroom, to avoid unnecessary navigation.
      s"/u/$userId/$spaceId/#$rest"
    } else
      MainDecorator.adjustUrl(urlIn)
  }
}
