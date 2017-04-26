package querki.publication

import querki.ecology._
import querki.globals._

import PublicationEvents._

object MOIDs extends EcotIds(68) {
  val CanPublishOID = moid(1)
  val CanReadAfterPublicationOID = moid(2)
  val PublishableModelOID = moid(3)
  val MinorUpdateOID = moid(4)
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
      
  // TODO: this should become an Internal Property, once we have a formal Publication page in the UI:
  lazy val PublishableModel = new SystemProperty(PublishableModelOID, YesNoType, ExactlyOne,
    toProps(
      setName("Is a Publishable Model"),
      Summary("Indicates that Instances of this Model will be formally Published when they are ready"),
      Details("""In some Spaces, particularly public ones such as blogs, FAQs, and other information sources,
        |you want to impose some structure when creating new Things. Instead of just making everything publicly
        |available as soon as you create them, you want to work on them for a while first, drafting them
        |privately, and then making them publicly available once they are ready. This step is called
        |Publication, and this Property makes it happen.
        |
        |If you have a Model that you want to publish like this, add this Property and turn it on. This
        |will change the workflow for Instances of the Model in a number of ways:
        |
        |* Newly-created Instances will have the Who Can Read permission set to the same value as the
        |Model itself. (You should usually set that to Editors only.)
        |* The Editor for Instances will gain a new button that lets you Publish that Instance.
        |* When it is published, the Instance will become Public. (This is the Who Can Read After Publications
        |permission; you can change it to something other than Public if you prefer.)
        |* The Space gains a Recent Changes page, which shows the Published Instances in Publication order.
        |* Once Published, Instances can later be formally Updated, which adds another entry to Recent Changes.
        |* The Space gains an RSS feed, so that Published Instances can be monitored from outside Querki. This
        |allows you to treat any sort of Querki information as a sort of blog.""".stripMargin)))
 
  lazy val MinorUpdateProp = new SystemProperty(MinorUpdateOID, YesNoType, ExactlyOne,
    toProps(
      setName("_minorUpdate"),
      setInternal,
      Summary("Iff set, this Update should be considered Minor."),
      Details("""This is the Property behind the "Minor Update" button in the Editor. It is an
        |internal meta-Property on the Publication event itself, rather than on the Thing.""".stripMargin)))

  override lazy val props = Seq(
    CanPublishPermission,
    CanReadAfterPublication,
    PublishableModel,
    MinorUpdateProp
  )
}
