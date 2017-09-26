package querki.conversations

import akka.actor.{ActorRef, Props}
import akka.pattern._

import models._
import querki.ecology._
import querki.globals._
import querki.identity.User
import querki.spaces.SpacePersistenceFactory
import querki.spaces.messages.SpaceSubsystemRequest
import querki.util.ActorHelpers
import querki.values.{QLContext, SpaceState}

import PersistentEvents._
import messages._

object MOIDs extends EcotIds(35) {
  val CommentTextOID = moid(1)
  val CanCommentPermOID = moid(2)
  val CanReadCommentsPermOID = moid(3)
  val ThingConversationsFunctionOID = moid(5)
  val CommentTypeOID = moid(6)
  val ConversationTypeOID = moid(7)
}
import MOIDs._

class ConversationEcot(e:Ecology) extends QuerkiEcot(e) with Conversations with querki.core.MethodDefs {
    
  val AccessControl = initRequires[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
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
  
  lazy val CommentType = new SystemType[Comment](CommentTypeOID, 
    toProps(
      setName("_commentType"),
      setInternal,
      Summary("Represents a single Comment in a Conversation.")
    )) with SimplePTypeBuilder[Comment]
  {
    def doDeserialize(ser:String)(implicit state:SpaceState):Comment = ???
    def doSerialize(v:Comment)(implicit state:SpaceState):String = ???
    def doWikify(context:QLContext)(v:Comment, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      fut(Wikitext(s"Comment: $props"))
    }
    def doDefault(implicit state:SpaceState):Comment = ???
    def doComputeMemSize(v:Comment):Int = 0
  }
  
  lazy val ConversationType = new SystemType[ConversationNode](ConversationTypeOID, 
    toProps(
      setName("_conversationType"),
      setInternal,
      Summary("Represents a particular Conversation or Thread.")
    )) with SimplePTypeBuilder[ConversationNode]
  {
    def doDeserialize(ser:String)(implicit state:SpaceState):ConversationNode = ???
    def doSerialize(v:ConversationNode)(implicit state:SpaceState):String = ???
    def doWikify(context:QLContext)(v:ConversationNode, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None):Future[Wikitext] = {
      fut(Wikitext(s"This is a single Conversation"))
    }
    def doDefault(implicit state:SpaceState):ConversationNode = ???
    def doComputeMemSize(v:ConversationNode):Int = 0
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
          SpaceSubsystemRequest(inv.context.request.requesterOrAnon, inv.state.id, GetConversations(t.id))).mapTo[ThingConversations])
        node <- inv.iter(convs.comments)
      }
        yield ExactlyOne(ConversationType(node))
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
  
  override lazy val props = Seq(
    ThingConversationsFunction,
    
    CommentText,
    CanComment,
    CanReadComments
  )
}
