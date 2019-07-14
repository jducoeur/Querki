package querki.conversations

import akka.actor.{ActorRef, Props}
import akka.pattern._

import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time.StaticDateTime

import models._
import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.spaces.SpacePersistenceFactory
import querki.spaces.messages.SpaceSubsystemRequest
import querki.time.DateTime
import querki.util.ActorHelpers
import querki.values.{QLContext, RequestContext, SpaceState}

import PersistentEvents._
import messages._

object MOIDs extends EcotIds(35) {
  val CommentTextOID = moid(1)
  val CanCommentPermOID = moid(2)
  val CanReadCommentsPermOID = moid(3)
  val ThingConversationsFunctionOID = moid(5)
  val CommentTypeOID = moid(6)
  val ConversationTypeOID = moid(7)
  val CommentsFunctionOID = moid(8)
  val LatestCommentTimeFunctionOID = moid(9)
  val CanModeratePermOID = moid(10)
}
import MOIDs._

class ConversationEcot(e:Ecology) extends QuerkiEcot(e) with Conversations with querki.core.MethodDefs {
    
  val AccessControl = initRequires[querki.security.AccessControl]
  val Time = initRequires[querki.time.Time]
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val Person = interface[querki.identity.Person]
  lazy val QL = interface[querki.ql.QL]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  implicit val timeout = ActorHelpers.timeout
  
  override def postInit() = {
    // Some entry points are legal without login:
    ApiRegistry.registerApiImplFor[ConversationFunctions, ConversationFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  override def persistentMessages = persist(35,
    (classOf[DHComment] -> 100),
    (classOf[DHNode] -> 101),
    (classOf[DHConvs] -> 102),
    (classOf[DHAddComment] -> 103),
    (classOf[DHDeleteComment] -> 104)
  )
  
  lazy val traceConv = Config.getBoolean("querki.test.traceConversations", false)
  def convTrace(msg: => String):Unit = {
    if (traceConv)
      QLog.spew(msg)
  }
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def conversationActorProps(persistenceFactory:SpacePersistenceFactory, spaceId:OID, space:ActorRef):Props = 
    Props(new SpaceConversationsActor(ecology, persistenceFactory, spaceId, space))
    
  def conversationsManagerProps(router:ActorRef):Props = Props(classOf[SpaceConversationsManager], ecology, router)
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def conversationPersisterProps(spaceId:OID):Props = 
    Props(new ConversationPersister(spaceId, ecology))
      
  def canReadComments(req:User, thingId:OID, state:SpaceState) = {
    AccessControl.hasPermission(CanReadComments, state, req, thingId)
  }
  
  def canWriteComments(identity:OID, thingId:OID, state:SpaceState) = {
    AccessControl.hasPermission(CanComment, state, identity, thingId)
  }
    
  /***********************************************
   * TYPES
   ***********************************************/
  
  def renderComment(t:Thing, c: Comment)(implicit rc: RequestContext, s: SpaceState): Future[Wikitext] = {
    if (c.isDeleted)
      fut(Wikitext("*Comment deleted*"))
    else {
      CommentText.firstOpt(c.props) match {
        case Some(comment) => {
          QL.process(comment, t.thisAsContext)
        }
        case _ => fut(Wikitext(""))
      }
    }
  }
  
  def renderComment(context:QLContext, v:Comment, isStandalone:Boolean):Future[Wikitext] = {
    val tOpt: Option[Thing] = context.state.anything(v.thingId)
    if (v.isDeleted || v.isArchived || tOpt.isEmpty) {
      fut(Wikitext(""))
    } else {
      val t = tOpt.get
      val authorName = Person.localPerson(v.authorId)(context.state).map(_.displayName).getOrElse("Anonymous")
      renderComment(t, v)(context.request, context.state).map { commentWikitext =>
        // We need to pre-render the wikitext, since we're going to be including it in-page:
        val commentString = commentWikitext.display.toString
        val commentCls = if (isStandalone) "_commentData" else "_convCommentData"
        val wikitext = s"""<div class="$commentCls" data-commentid="${v.id}" data-thingid="${v.thingId.toThingId}" data-authorid="${v.authorId.toThingId}" data-authorname="$authorName" data-time="${v.createTime.getMillis}">
                        |$commentString
                        |</div>""".stripMargin
        HtmlWikitext(wikitext)
      }
    }    
  }
  
  lazy val CommentType = new SystemType[Comment](CommentTypeOID, 
    toProps(
      setName("_commentType"),
      setInternal,
      Summary("Represents a single Comment in a Conversation.")
    )) with SimplePTypeBuilder[Comment]
  {
    def doDeserialize(ser:String)(implicit state:SpaceState): Comment = ???
    def doSerialize(v: Comment)(implicit state:SpaceState):String = ???
    def doWikify(context:QLContext)(v: Comment, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      renderComment(context, v, true)
    }
    def doDefault(implicit state:SpaceState): Comment = ???
    def doComputeMemSize(v: Comment):Int = 0
  }
  
  lazy val ConversationType = new SystemType[ConversationNode](ConversationTypeOID, 
    toProps(
      setName("_conversationType"),
      setInternal,
      Summary("Represents a particular Conversation or Thread.")
    )) with SimplePTypeBuilder[ConversationNode]
  {
    def doDeserialize(ser:String)(implicit state:SpaceState): ConversationNode = ???
    def doSerialize(v: ConversationNode)(implicit state:SpaceState):String = ???
    def doWikify(context:QLContext)(v: ConversationNode, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      val canComment = 
        (for {
          request <- context.requestOpt
          user <- request.requester
        }
          yield AccessControl.hasPermission(CanComment, context.state, user, v.comment.thingId)
        ).getOrElse(false)
      val comments = collectComments(v, List.empty)
      val commentsFut:Future[List[Wikitext]] = Future.sequence(comments.map(renderComment(context, _, false)))
      for {
        commentWikitexts <- commentsFut
        commentsWikitext = commentWikitexts.reduce(_ + _)
      }
        yield 
          HtmlWikitext(s"""<div class="_conversationData" data-thingid="${v.comment.thingId.toThingId}" data-cancomment="$canComment">""") + 
          commentsWikitext + 
          HtmlWikitext("</div>")
    }
    def doDefault(implicit state:SpaceState): ConversationNode = ???
    def doComputeMemSize(v: ConversationNode):Int = 0
  }
  
  override lazy val types = Seq(
    CommentType,
    ConversationType
  )
    
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  lazy val ThingConversationsFunction = new InternalMethod(ThingConversationsFunctionOID,
      toProps(
        setName("_thingConversations"),
        Summary("""Produces the Conversations on this Thing, if any."""),
        Categories(ConvTag),
        Signature(
          expected = Some(Seq(LinkType), "A Thing"),
          reqs = Seq.empty,
          opts = Seq.empty,
          returns = (ConversationType, "A List of the Conversations existing on that Thing, if there are any.")
        ),
        Details("""Conversations show up on a Thing's page, normally. But sometimes you want to be able
          |to show them elsewhere, including on other pages. This is how you get at them, after which
          |you can display them as you please.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        t <- inv.contextAllThings
        convs <- inv.fut((SpaceOps.spaceRegion ? 
          SpaceSubsystemRequest(inv.context.request, inv.state.id, GetConversations(t.id))).mapTo[ThingConversations])
        node <- inv.iter(convs.comments)
      }
        yield ExactlyOne(ConversationType(node))
    }
  }
  
  /**
   * Since a Thread is essentially a tree, and the top-level Comments are basically the
   * left-hand branch, we need to recurse down to collect them.
   */
  @annotation.tailrec
  final def collectComments(conv:ConversationNode, soFar:List[Comment]):List[Comment] = {
    val result = conv.comment :: soFar
    conv.responses.headOption match {
      case None => result.reverse
      case Some(resp) => collectComments(resp, result)
    }
  }
  
  lazy val CommentsFunction = new InternalMethod(CommentsFunctionOID,
      toProps(
        setName("_comments"),
        Summary("""Produces the Comments in this Conversation or Thread"""),
        Categories(ConvTag),
        Signature(
          expected = Some(Seq(ConversationType), "A Conversation or Thread"),
          reqs = Seq.empty,
          opts = Seq.empty,
          returns = (CommentType, "A List of the *primary* Comments in this Thread.")
        ),
        Details("""Given a single Conversation, this lets you get the individual Comments. However, note that this is
                  |just the flat top-level thread; if there are sub-Threads (once Querki has sub-Threads), you will
                  |have to get those from the Comments they hang off of.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        conv <- inv.contextAllAs(ConversationType)
        comment <- inv.iter(collectComments(conv, List.empty))
      }
        yield ExactlyOne(CommentType(comment))
    }
  }
  
  // TBD: not tail-recursive, because tail-recursive on trees is a pain. Might want to put in the
  // work to make it so, though.
  private def latestCommentTime(conv:ConversationNode):DateTime = {
    (conv.comment.createTime /: conv.responses) { (curBest, resp) =>
      val respTime = latestCommentTime(resp)
      if (respTime > curBest) {
        respTime
      } else {
        curBest
      }
    }
  }
  
  lazy val LatestCommentTimeFunction = new InternalMethod(LatestCommentTimeFunctionOID,
    toProps(
      setName("_latestCommentTime"),
      Summary("""Given a Conversation, this returns the Time of the most recent comment. Mainly used for sorting Conversations."""),
      Categories(ConvTag),
      Signature(
        expected = Some(Seq(ConversationType), "A Conversation or Thread"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (Time.QDateTime, "The time of the most recent comment in that Conversation.")
      )))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        conv <- inv.contextAllAs(ConversationType)
        time = latestCommentTime(conv)
      }
        yield ExactlyOne(Time.QDateTime(time))
    }
  }
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  /**
   * TODO: this shouldn't really be PlainText -- it should be a Type that is explicitly Wikitext for now.
   * Model this on QL.ParsedTextType, but make it realer.
   */
  lazy val CommentText = new SystemProperty(CommentTextOID, TextType, Optional,
      toProps(
        setName("Comment Text"),
        setInternal))
  
  lazy val CanComment = AccessControl.definePermission(CanCommentPermOID, "Who Can Comment", "Who can comment on this Thing (or generally in this Space)",
      Seq(AccessControl.OwnerTag, AccessControl.MembersTag),
      Seq(AccessControl.AppliesToSpace, AccessControl.AppliesToModels, AccessControl.AppliesToInstances),
      true, false)
      
  lazy val CanReadComments = AccessControl.definePermission(CanReadCommentsPermOID, "Who Can Read Comments", "Who can read the comments on this Thing?",
      Seq(AccessControl.OwnerTag, AccessControl.PublicTag), 
      Seq(AccessControl.AppliesToSpace, AccessControl.AppliesToModels, AccessControl.AppliesToInstances),
      true, true)
      
  lazy val CanModerate = AccessControl.definePermission(CanModeratePermOID, "Who Can Moderate", "Who can moderate comments on this Thing", 
      Seq(AccessControl.OwnerTag), 
      Seq(AccessControl.AppliesToSpace, AccessControl.AppliesToModels, AccessControl.AppliesToInstances), 
      true, false)
  
  override lazy val props = Seq(
    ThingConversationsFunction,
    CommentsFunction,
    LatestCommentTimeFunction,
    
    CommentText,
    CanComment,
    CanReadComments,
    CanModerate
  )
}
