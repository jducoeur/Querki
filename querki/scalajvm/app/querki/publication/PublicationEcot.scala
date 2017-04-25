package querki.publication

import querki.ecology._
import querki.globals._

import PublicationEvents._

object MOIDs extends EcotIds(68) {
  val CanPublishOID = moid(1)
  val CanReadAfterPublicationOID = moid(2)
}

class PublicationEcot(e:Ecology) extends QuerkiEcot(e) with Publication {
  
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  
  override def persistentMessages = persist(68,
    (classOf[PublishEvent] -> 100),
    (classOf[PublishedThingInfo] -> 101)
  )
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val CanPublishPermission = AccessControl.definePermission(
      CanPublishOID, 
      "Who Can Publish", 
      "Who is allowed to Publish Instances in this Space",
      Seq(AccessControl.OwnerTag),
      Seq(AccessControl.AppliesToInstances),
      false, false)
  
  lazy val CanReadAfterPublication = AccessControl.definePermission(
      CanReadAfterPublicationOID, 
      "Who Can Read After Publication", 
      "After an Instance has been Published, who can read it?",
      Seq(AccessControl.PublicTag),
      Seq(AccessControl.AppliesToInstances),
      false, true)

  override lazy val props = Seq(
    CanPublishPermission,
    CanReadAfterPublication
  )
}
