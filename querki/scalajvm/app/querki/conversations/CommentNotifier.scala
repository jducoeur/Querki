package querki.conversations

import models.{HtmlWikitext, PType, Wikitext}

import querki.ecology._
import querki.globals._
import querki.html.QHtml
import querki.identity.User
import querki.notifications._
import querki.uservalues.PersistMessages.OneUserValue
import querki.util.QLog
import querki.values.{QLContext, SpaceState}

import messages.Comment

private [conversations] object CommentNotifierMOIDs extends EcotIds(50) {
  val CommentThingNameOID = moid(1)
  val CommentBodyOID = moid(2)
  val CommentIdOID = moid(3)
  val CommentSpaceOwnerOID = moid(4)
  val GetCommentNotesOID = moid(5)
}

class CommentNotifierEcot(e:Ecology) extends QuerkiEcot(e) with Notifier with NotifyComments {
  import CommentNotifierMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  val UserValues = initRequires[querki.uservalues.UserValues]
  
  lazy val Conversations = interface[Conversations]
  lazy val Notifications = interface[querki.notifications.Notifications]
  lazy val NotifierRegistry = interface[querki.notifications.NotifierRegistry]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val System = interface[querki.system.System]
  
  lazy val CommentText = Conversations.CommentText
  lazy val PlainTextType = Basic.PlainTextType
  lazy val SystemState = System.State
  
  override def postInit() = {
    NotifierRegistry.register(this)
  }
  
  override def term() = {
    NotifierRegistry.unregister(this)
  }

  object Notifiers {
    val CommentNotifierId:Short = 1
  }
  
  def id = NotifierId(MOIDs.ecotId, Notifiers.CommentNotifierId)

  // TODO: we're not summarizing on the first pass, but should be doing so:
  def summarizeAt:SummarizeAt.SummarizeAt = SummarizeAt.None
  
  def summarizeNew(context:QLContext, notes:Seq[Notification]):Future[SummarizedNotifications] = {
    if (notes.length != 1)
      throw new Exception("CommentNotifier.summarizeNew current expects exactly one notification at a time!")
      
    val note = notes.head
    render(context, note) map { rendered =>
      SummarizedNotifications(rendered.headline, rendered.content, notes)      
    }
  }
    
  def notifyComment(req:User, comment:Comment, commentNotifyPrefs:Seq[OneUserValue])(implicit state:SpaceState) = {
    val thingNameOpt = for {
      thing <- state.anything(comment.thingId)
    }
      yield thing.displayName
      
    val bodyOpt = for {
      qv <- comment.props.get(CommentText.id)
      body <- qv.firstAs(TextType)
    }
      yield body
    
    val payload = toProps(
        CommentThingName(thingNameOpt.get),
        // NOTE: yes, CommentBody seems redundant with CommentText. But in the medium term, we plan to allow QL in
        // CommentText, and that is absolutely *not* allowed in the contents of a Notification. So we will need to
        // run the QL here, and put the results into CommentBody.
        CommentBody(bodyOpt.map(_.text).getOrElse("")),
        CommentId(comment.id),
        CommentSpaceOwner(state.owner)
      )
    // IMPORTANT TODO: we're currently generating notifications as already Read, because there is no way for the
    // user to mark them as Read yet. Once we have the needed UI, change the parameter below.
    val note = Notification(
      EmptyNotificationId,
      comment.authorId, 
      None,
      id,
      comment.createTime,
      Some(comment.spaceId), 
      Some(comment.thingId), 
      SpacePersistence.serProps(payload, state),
      true,
      false)
      
    val recipients = (Set(state.owner) /: commentNotifyPrefs) { (recip, onePref) => 
      if (onePref.thingId == state.id || onePref.thingId == comment.thingId) {
        onePref.v.firstAs(YesNoType) match {
          case Some(bool) => {
            if (bool) {
              recip + onePref.identity.id
            } else {
              recip - onePref.identity.id
            }
          }
          case None => {
            QLog.error("CommentNotifier got a commentNotifyPref that isn't a YesNo: " + onePref)
            recip
          }
        }
      } else {
        // It's not relevant to this comment
        recip
      }
    }
      
    // Don't send the notification to the person who wrote the comment:
    // TODO: all members of the Space should be able to opt into receiving comments, and the owner should
    // be able to opt out:
    val recipientsNotAuthor = recipients.filterNot(_ == comment.authorId).toSeq
    
    Notifications.send(req, ExplicitRecipients(recipientsNotAuthor), note)
  }
  
  // TODO: this should become a standard utility:
  class RichPropMap(payload:PropMap) {
    def getProp[VT](prop:Property[VT,_], as:PType[VT]):Option[VT] = {
      for {
        qv <- payload.get(prop)
        v <- qv.firstAs(as)
      }
        yield v
    }
  }
  implicit def payload2Rich(payload:PropMap):RichPropMap = new RichPropMap(payload)
  
  def commentLink(note:Notification):Option[String] = {
    val rawPayload = note.payload
    val payload = SpacePersistence.deserProps(rawPayload, SystemState)
    for {
      ownerId <- payload.getProp(CommentSpaceOwner, LinkType)
      spaceId <- note.spaceId
      thingId <- note.thingId
      commentId <- payload.getProp(CommentId, IntType)
    }
      // TODO: ideally, this path shouldn't be hardcoded like this. But we don't really expect it to change:
      // TODO: more importantly, this should be producing true ThingIds, not OIDs; this currently causes bad
      // URLs. Fixing that is probably going to require getting the pipeline to work async, though, so we can
      // fetch the names.
      yield s""" <a href="/u/${ownerId.toThingId}/${spaceId.toThingId}/${thingId.toThingId}#comment$commentId" title="Click to go to this comment">""" +
             """<i class="glyphicon glyphicon-share-alt"></i></a>"""
  }
    
  def render(context:QLContext, note:Notification):Future[RenderedNotification] = {
    val rawPayload = note.payload
    val payload = SpacePersistence.deserProps(rawPayload, SystemState)
    val resultOpt = for {
      thingName <- payload.getProp(CommentThingName, PlainTextType)
      link = commentLink(note)
      header = s"""Comment on "${thingName.text}"""" + commentLink(note).getOrElse("")
      bodyQV <- payload.get(CommentBodyOID)
      body = bodyQV.wikify(context)
    }
      yield body.map(RenderedNotification(HtmlWikitext(QHtml(header)), _))
        
    resultOpt.getOrElse {
      QLog.error("CommentNotifier got badly-formed Notification: " + note)
      Future.successful(RenderedNotification(Wikitext("INTERNAL ERROR"), Wikitext("We're sorry, but this message seems to have gotten messed up")))
    }
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val CommentThingName = new SystemProperty(CommentThingNameOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_commentNotifyThingName"),
      setInternal))
  
  lazy val CommentBody = new SystemProperty(CommentBodyOID, PlainTextType, ExactlyOne,
    toProps(
      setName("_commentNotifyBody"),
      setInternal))
  
  lazy val CommentId = new SystemProperty(CommentIdOID, IntType, ExactlyOne,
    toProps(
      setName("_commentNotifyId"),
      setInternal))
  
  lazy val CommentSpaceOwner = new SystemProperty(CommentSpaceOwnerOID, LinkType, ExactlyOne,
    toProps(
      setName("_commentNotifySpaceOwner"),
      setInternal))
  
  lazy val GetCommentNotesPref = new SystemProperty(GetCommentNotesOID, YesNoType, Optional,
    toProps(
      setName("_getCommentNotifications"),
      setInternal,
      UserValues.IsUserValueFlag(true)))
  
  override lazy val props = Seq(
    CommentThingName,
    CommentBody,
    CommentId,
    CommentSpaceOwner,
    GetCommentNotesPref
  )
}
