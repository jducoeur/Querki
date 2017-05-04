package querki.publication

import scala.concurrent.duration._
import akka.pattern._
import akka.util.Timeout

import com.github.nscala_time.time.Imports.DateTimeFormat

import models._
import querki.api.commonName
import querki.ecology._
import querki.globals._
import querki.spaces.messages._
import querki.values.QLContext

import PublicationCommands._
import PublicationEvents._

object MOIDs extends EcotIds(68) {
  val CanPublishOID = moid(1)
  val CanReadAfterPublicationOID = moid(2)
  val PublishableModelOID = moid(3)
  val MinorUpdateOID = moid(4)
  val PublishedOID = moid(5)
  val UnpublishedChangesOID = moid(6)
  val GetChangesOID = moid(7)
  val PublishEventTypeOID = moid(8)
}

class PublicationEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with Publication {
  
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  val Time = initRequires[querki.time.Time]
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[PublicationFunctions, PublicationFunctionsImpl](SpaceOps.spaceRegion)
  }
  
  override def persistentMessages = persist(68,
    (classOf[PublishEvent] -> 100),
    (classOf[PublishedThingInfo] -> 101)
  )
  
  val PublicationTag = "Publication"

  /******************************************
   * TYPES
   ******************************************/
  
  lazy val PublishEventType = new SystemType[OnePublishEvent](PublishEventTypeOID,
    toProps(
      setName("_publishEventType"),
      Categories(PublicationTag),
      Core.InternalProp(true),
      Summary("Represents a single Publish or Update event"))) with SimplePTypeBuilder[OnePublishEvent]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = ???
    def doSerialize(v:OnePublishEvent)(implicit state:SpaceState) = ???
    val defaultRenderFormat = DateTimeFormat.forPattern("MM/dd/yyyy")
    
    def doWikify(context:QLContext)(v:OnePublishEvent, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      // TODO: for the moment, this is assuming exactly one Thing per Event. That may not always be the case:
      val thingInfo = v.things.head
      val when = defaultRenderFormat.print(v.when)
      val eventKind = if (thingInfo.isUpdate) "updated" else "published"
      Future.successful(Wikitext(s"* $when: [${thingInfo.displayName}](${thingInfo.thingId.toThingId.toString}) $eventKind"))
    }
    
    def doDefault(implicit state:SpaceState) = ???
    
    def doComputeMemSize(v:OnePublishEvent):Int = 0
  }

  /******************************************
   * FUNCTIONS
   ******************************************/
  
  lazy val GetChangesFunction = new InternalMethod(GetChangesOID,
    toProps(
      setName("_getChanges"),
      Categories(PublicationTag),
      Summary("Fetch the Publication history of this Space, so you can see what has changed."),
      Signature(
        expected = None,
        reqs = Seq.empty,
        opts = Seq(
          ("since", Time.QDate, Core.QNone, "When the changes should start"),
          ("until", Time.QDate, Core.QNone, "When the changes should end"),
          ("changesTo", LinkType, Core.QNone, "The Model(s) to include in the list"),
          ("includeMinor", YesNoType, ExactlyOne(YesNoType(false)), "If true, include Minor changes in the list"),
          ("reverse", YesNoType, ExactlyOne(YesNoType(false)), "If true, list the results in reverse-chronological order, with the most recent first")
        ),
        returns = (PublishEventType, "A series of Publication Events, in chronological order.")
      ),
      Details("Fill this in!")))
  {
    override def qlApply(inv:Invocation):QFut = {
      // TODO: this is annoyingly hard-coded. Think this through -- how long should it be? Should it
      // be configurable? (Probably.)
      implicit val timeout = Timeout(10 seconds)
      for {
        since <- inv.processAsOpt("since", Time.QDate)
        until <- inv.processAsOpt("until", Time.QDate)
        changesTo <- inv.processAsList("changesTo", LinkType)
        includeMinor <- inv.processAs("includeMinor", YesNoType)
        reverse <- inv.processAs("reverse", YesNoType)
        who = inv.context.request.requesterOrAnon
        cmd = SpaceSubsystemRequest(who, inv.state.id, GetEvents(who, since, until, includeMinor, false))
        RequestedEvents(events) <- inv.fut(SpaceOps.spaceRegion ? cmd)
        orderedEvents = if (reverse) events.reverse else events
        filteredEvents = filterOnModels(changesTo, orderedEvents)(inv.state)
      }
        yield QList.makePropValue(filteredEvents.map(PublishEventType(_)), PublishEventType)
    }
    
    def filterOnModels(changesTo:List[OID], events:Seq[OnePublishEvent])(implicit state:SpaceState):Seq[OnePublishEvent] = {
      if (changesTo.isEmpty)
        events
      else {
        events.filter { event =>
          event.things.exists { thingInfo =>
            state.anything(thingInfo.thingId).map { thing =>
              changesTo.contains(thing.model)
            }.getOrElse(false)
          }
        }
      }
    }
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val CanPublishPermission = AccessControl.definePermission(
      CanPublishOID, 
      commonName(_.publication.canPublishPerm), 
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
  lazy val PublishableModelProp = new SystemProperty(PublishableModelOID, YesNoType, ExactlyOne,
    toProps(
      setName(commonName(_.publication.publishableProp)),
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
  
  lazy val PublishedProp = new SystemProperty(PublishedOID, YesNoType, ExactlyOne,
    toProps(
      setName(commonName(_.publication.publishedProp)),
      setInternal,
      Summary("Set to true when this Instance gets Published.")))
  
  lazy val HasUnpublishedChanges = new SystemProperty(UnpublishedChangesOID, YesNoType, ExactlyOne,
    toProps(
      setName(commonName(_.publication.hasUnpublishedChangesProp)),
      setInternal,
      Summary("Set to true iff this Thing has been Published, then edited but not yet Updated.")))

  override lazy val props = Seq(
    GetChangesFunction,
    
    CanPublishPermission,
    CanReadAfterPublication,
    PublishableModelProp,
    MinorUpdateProp,
    PublishedProp,
    HasUnpublishedChanges
  )
}
