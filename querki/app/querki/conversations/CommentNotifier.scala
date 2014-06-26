package querki.conversations

import models.Wikitext

import querki.ecology._
import querki.identity.User
import querki.notifications._
import querki.util.QLog
import querki.values.{QLContext, SpaceState}

import messages.Comment

private [conversations] object CommentNotifierMOIDs extends EcotIds(50) {
  val CommentThingNameOID = moid(1)
  val CommentBodyOID = moid(2)
}

private [conversations] trait NotifyComments extends EcologyInterface {
  /**
   * When a new comment is created, call this to send out notifications.
   */
  def notifyComment(req:User, comment:Comment)(implicit state:SpaceState)
}

class CommentNotifierEcot(e:Ecology) extends QuerkiEcot(e) with Notifier with NotifyComments {
  import CommentNotifierMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  
  lazy val Conversations = interface[Conversations]
  lazy val Notifications = interface[querki.notifications.Notifications]
  lazy val NotifierRegistry = interface[querki.notifications.NotifierRegistry]
  
  lazy val CommentText = Conversations.CommentText
  lazy val PlainTextType = Basic.PlainTextType
  
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
  
  def summarizeNew(context:QLContext, notes:Seq[Notification]):SummarizedNotifications = {
    if (notes.length != 1)
      throw new Exception("CommentNotifier.summarizeNew current expects exactly one notification at a time!")
      
    val note = notes.head
    val rendered = render(context, note)
    SummarizedNotifications(rendered.headline, rendered.content, notes)
  }
    
  def notifyComment(req:User, comment:Comment)(implicit state:SpaceState) = {
    val thingNameOpt = for {
      thing <- state.anything(comment.thingId)
    }
      yield thing.displayName
      
    val bodyOpt = for {
      qv <- comment.props.get(CommentText.id)
      body <- qv.firstAs(Basic.PlainTextType)
    }
      yield body
    
    val payload = toProps(
        CommentThingName(thingNameOpt.get),
        // NOTE: yes, CommentBody seems redundant with CommentText. But in the medium term, we plan to allow QL in
        // CommentText, and that is absolutely *not* allowed in the contents of a Notification. So we will need to
        // run the QL here, and put the results into CommentBody.
        CommentBody(bodyOpt.get.text)
      )()
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
      payload,
      true,
      false)
      
    // Don't send the notification to the person who wrote the comment:
    // TODO: all members of the Space should be able to opt into receiving comments, and the owner should
    // be able to opt out:
    val recipients = Seq(state.owner).filterNot(_ == comment.authorId)
    
    Notifications.send(req, ExplicitRecipients(recipients), note)
  }
    
  def render(context:QLContext, note:Notification):RenderedNotification = {
    val payload = note.payload
    val resultOpt = for {
      thingNameQV <- payload.get(CommentThingNameOID)
      thingName <- thingNameQV.firstAs(PlainTextType)
      header = s"""Comment on "${thingName.text}""""
      bodyQV <- payload.get(CommentBodyOID)
      body = bodyQV.wikify(context)
    }
      yield RenderedNotification(Wikitext(header), body)
        
    resultOpt.getOrElse {
      QLog.error("CommentNotifier got badly-formed Notification: " + note)
      RenderedNotification(Wikitext("INTERNAL ERROR"), Wikitext("We're sorry, but this message seems to have gotten messed up"))
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
  
  override lazy val props = Seq(
    CommentThingName,
    CommentBody
  )
}
