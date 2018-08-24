package querki.notifications

import models.{HtmlWikitext, emptyProps, Wikitext}
import querki.ecology._
import querki.email._
import EmailFunctions.UnsubOption
import querki.globals._
import querki.identity.FullIdentity
import querki.persistence.UseKryo
import querki.time.DateTime
import querki.util.SafeUrl
import querki.values.{ShowLinksAsFullAnchors, QLContext}

private [notifications] object UserlandNotifierMOIDs extends EcotIds(74) {
  val NotifyMethodOID = moid(1)
  val NotifyURLOID = moid(2)
  val NotifySubjectOID = moid(3)
  val NotifyBodyOID = moid(4)
  val NotifySpaceNameOID = moid(5)
  val NotifySpaceOwnerOID = moid(6)
  val NotifyThingOID = moid(7)
  val NotifyTopicOID = moid(8)
}

class UserlandNotifierEcot(e:Ecology) extends QuerkiEcot(e) with Notifier with EmailNotifier with querki.core.MethodDefs {
  import UserlandNotifierMOIDs._

  val Basic = initRequires[querki.basic.Basic]
  val QL = initRequires[querki.ql.QL]
  
  lazy val Links = interface[querki.links.Links]
  lazy val Notifications = interface[querki.notifications.Notifications]
  lazy val NotifierRegistry = interface[querki.notifications.NotifierRegistry]
  lazy val Person = interface[querki.identity.Person]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val System = interface[querki.system.System]
  
  lazy val ParsedTextType = QL.ParsedTextType
  lazy val PlainTextType = Basic.PlainTextType
  lazy val SystemState = System.State
  
  lazy val urlBase = Config.getString("querki.app.urlRoot")

  override def postInit() = {
    NotifierRegistry.register(this)
  }
  
  override def term() = {
    NotifierRegistry.unregister(this)
  }

  object Notifiers {
    val UserlandNotifierId:Short = 1
  }
  
  val userlandNotifierId = NotifierId(UserlandNotifierMOIDs.ecotId, Notifiers.UserlandNotifierId)
  def id = userlandNotifierId
  
  /***********************************************
   * Implementation of Notifier
   ***********************************************/

  // TBD: in theory, we should probably summarize based on the "topic" provided in the _notify() function.
  // But I'm not sure there is a good way to do that.
  def summarizeAt:SummarizeAt.SummarizeAt = SummarizeAt.None
  
  def summarizeNew(context:QLContext, notes:Seq[Notification]):Future[SummarizedNotifications] = {
    if (notes.length != 1)
      throw new Exception("UserlandNotifier.summarizeNew current expects exactly one notification at a time!")
      
    val note = notes.head
    render(context, note) map { rendered =>
      SummarizedNotifications(rendered.headline, rendered.content, notes)      
    }
  }
  
  case class UserlandPayload(subject: String, body: Wikitext, spaceName: String, spaceOwner: String, thing: Option[OID], topic: Option[String])
  def parsePayload(note: Notification): UserlandPayload = {
    val rawPayload = note.payload
    val payload = SpacePersistence.deserProps(rawPayload, SystemState)
    
    UserlandPayload(
      payload.getFirst(NotifySubjectProp).text,
      payload.getFirst(NotifyBodyProp),
      payload.getFirst(NotifySpaceNameProp).text,
      payload.getFirst(NotifySpaceOwnerProp).text,
      payload.getFirstOpt(NotifyThingProp),
      payload.getFirstOpt(NotifyTopicProp).map(_.text)
    )
  }
  
  def noteUrl(payload: UserlandPayload): String = {
    val thingId = payload.thing.map(_.toThingId).getOrElse("")
    
    urlBase + 
      "u/" + payload.spaceOwner + 
      "/" + payload.spaceName +
      "/" + thingId
  }
  
  def render(context:QLContext, note:Notification):Future[RenderedNotification] = {
    val payload = parsePayload(note)
    
    fut(RenderedNotification(
      // Use a raw HtmlWikitext, so that it doesn't open a new window:
      HtmlWikitext(s"""<a href="${noteUrl(payload)}">${payload.subject}</a>"""),
      payload.body
    ))
  }
  
  def emailNotifier = None //Some(this)
  
  /***********************************************
   * Implementation of EmailNotifier
   ***********************************************/
  
  def shouldSendEmail(note:Notification, unsubs:List[UnsubEvent]):Boolean = {
    // TODO: this should be true unless unsubbed:
    false
  }
  
  def toEmail(note:Notification, recipient:FullIdentity):Future[EmailMsg] = {
    ???
  }
  
  // TODO: we should have unsub options based on this Space, and this Topic
  def unsubOptions(unsubInfo:UnsubInfo):Future[(Wikitext, Seq[UnsubOption])] = {
    ???
  }
  
  def getUnsubEvent(unsubId:OID, context:Option[String]):(Wikitext, UnsubEvent with UseKryo) = {
    ???
  }

  /***********************************************
   * FUNCTIONS
   ***********************************************/

  lazy val NotifyMethod = new InternalMethod(NotifyMethodOID,
    toProps(
      setName("_notify"),
      Summary("Sends a notification to one or more Members of this Space"),
      Categories(NotifyTag),
      Signature(
        expected = Some(Seq(AnyType), "Anything -- this will only be used by the parameters"),
        reqs = Seq(
          ("recipients", LinkType, "The Members to receive this notification"),
          ("subject", ParsedTextType, "The subject line for the notification"),
          ("body", ParsedTextType, "The body of the notification. Can be arbitrary QL.")
        ),
        opts = Seq(
          ("thing", LinkType, Core.QNone, "If specified, the Notification will link directly to this Thing. If not, it will link to the Space root."),
          ("topic", ParsedTextType, Core.QNone, "If specified, noifications with the same topic are grouped conceptually, letting recipients manage them better.")
        ),
        returns = (IntType, "The number of people this was sent to.")
      ),
      Details("""Notifications are Querki's concept of messages. Using this function, you can cause QL expressions to send
                |messages to people when certain events happen. The `recipients`, `subject` and `body` are pretty much as you
                |would expect from email.
                |
                |The header of a Notification is a link. If you specify a `thing`, that is what the link will go to; otherwise, it
                |will go to the Space Root.
                |
                |Notifications *may* be sent as email, but don't count on that. It is strictly up to the recipient to decide what they
                |want to get as email. If a recipient unsubscribes from certain notifications by email, they will still receive them
                |in Querki's Notifications page. (Note: we will eventually provide capabilities to block notifications entirely, if
                |the recipient so chooses.)
                |
                |The `topic` parameter is optional, but can be helpful. It should name the general category of this *kind* of message.
                |Recipients can decide to unsubscribe a certain topic from email, while still receiving other notifications by email.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      implicit val state = inv.state
      val sender = inv.context.request.requesterOrAnon.mainIdentity
      
      for {
        recipientPersonIds <- inv.processAsList("recipients", LinkType)
        recipientPersons = recipientPersonIds.map(state.anything(_)).flatten
        recipientIdentities = recipientPersons.map(Person.getPersonIdentity(_)).flatten
        subject <- inv.processAs("subject", ParsedTextType)
        // We need links below this to be expressed fully, so they work in email and in Notifications
        // when viewed from outside this Space:
        body <- inv.processAs("body", ParsedTextType, inv.context.withFlag(ShowLinksAsFullAnchors))
        thing <- inv.processAsOpt("thing", LinkType)
        topic <- inv.processAsOpt("topic", ParsedTextType)
        
        payload = toProps(
          NotifySubjectProp(subject.strip),
          NotifyBodyProp(body),
          NotifySpaceNameProp(SafeUrl.apply(state.name)),
          NotifySpaceOwnerProp(state.ownerHandle)
        ) ++ 
          thing.map(t => toProps(NotifyThingProp(t))).getOrElse(emptyProps) ++ 
          topic.map(tp => toProps(NotifyTopicProp(tp.strip))).getOrElse(emptyProps) 
          
        note = Notification(
          EmptyNotificationId,
          sender.id, 
          None,
          userlandNotifierId,
          DateTime.now,
          Some(state.id), 
          thing, 
          SpacePersistence.serProps(payload, state),
          true,
          false)
          
        _ = Notifications.send(inv.context.request.requesterOrAnon, ExplicitRecipients(recipientIdentities), note)
      }
        yield ExactlyOne(IntType(recipientIdentities.length))
    }
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  lazy val NotifySubjectProp = new SystemProperty(NotifySubjectOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_notifyRecordSubject"),
      setInternal,
      Summary("The Subject of this userland _notify()")))

  // We actually store the body as wikitext, since it is potentially long and complex:
  lazy val NotifyBodyProp = new SystemProperty(NotifyBodyOID, ParsedTextType, ExactlyOne,
    toProps(
      setName("_notifyRecordBody"),
      setInternal,
      Summary("The Body of this userland _notify()")))

  lazy val NotifySpaceNameProp = new SystemProperty(NotifySpaceNameOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_notifyRecordSpaceName"),
      setInternal,
      Summary("The Name of the Space of this userland _notify()")))

  lazy val NotifySpaceOwnerProp = new SystemProperty(NotifySpaceOwnerOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_notifyRecordSpaceOwner"),
      setInternal,
      Summary("The Identity of the Space Owner of this userland _notify()")))

  lazy val NotifyThingProp = new SystemProperty(NotifyThingOID, LinkType, Optional,
    toProps(
      setName("_notifyRecordThing"),
      setInternal,
      Summary("The optional Thing from this userland _notify()")))

  lazy val NotifyTopicProp = new SystemProperty(NotifyTopicOID, PlainTextType, Optional,
    toProps(
      setName("_notifyRecordTopic"),
      setInternal,
      Summary("The optional Topic of this userland _notify()")))
  
  override lazy val props = Seq(
    NotifyMethod,
    
    NotifySubjectProp,
    NotifyBodyProp,
    NotifySpaceNameProp,
    NotifySpaceOwnerProp,
    NotifyThingProp,
    NotifyTopicProp
  )
}
