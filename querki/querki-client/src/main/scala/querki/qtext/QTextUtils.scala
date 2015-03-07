package querki.qtext

import scala.scalajs.js

import querki.globals._

class QTextUtilsEcot(e:Ecology) extends ClientEcot(e) with QTextUtils {
  
  def implements = Set(classOf[QTextUtils])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  /**
   * RegExp for old-fashioned paths into Querki Spaces.
   */
  val spacePathRegExp = js.RegExp("/u/([\\.\\w\\-]+)/([\\.\\w\\-]+)/([^#].+)")
  
  def adjustUrl(urlIn:String):String = {
    if (spacePathRegExp.test(urlIn)) {
      // This is a server-generated URL that is pointing into user space. We may need to tweak it,
      // but first we need to deconstruct it.
      val matches = spacePathRegExp.exec(urlIn)
      val userIdIn = matches(1)
      val spaceIdIn = matches(2)
      val rest = matches(3)
      // Detect if the userId is the OID of the current Client, and change to ThingId if so.
      // Ditto for the spaceId. Try to change /u/.0d9djkd/.98dk398d/stuff to /u/Joe/Workroom/stuff,
      // if we are currently looking at Joe/Workroom, to avoid unnecessary navigation.
      val (userId, spaceId) = DataAccess.space match {
        case Some(space) => {
          val u = if (userIdIn.isDefined && userIdIn.get == space.ownerId) space.ownerHandle else userIdIn.get
          val s = if (spaceIdIn.isDefined && spaceIdIn.get == space.oid.underlying && space.linkName.isDefined) space.linkName.get else spaceIdIn.get
          (u, s)
        }
        case None => { (userIdIn.getOrElse(""), spaceIdIn.getOrElse("")) }
      }
      s"/u/$userId/$spaceId/#$rest"
    } else
      MainDecorator.adjustUrl(urlIn)
  }
}
