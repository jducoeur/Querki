package querki.conversations

import org.scalatest.Assertions._

import querki.ecology._
import querki.identity._
import querki.identity.UserLevel._

import querki.test._

class ConversationPermissionTests extends QuerkiTests {
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Conversations = interface[querki.conversations.Conversations]

  def owner(implicit space: CommonSpace): User = space.owner
  def member(implicit space: CommonSpace): User = space.member1.user
  def nonMember(implicit space: CommonSpace): User = space.nonMember

  def canRead(who: User)(implicit space: CommonSpace): Boolean = {
    Conversations.canReadComments(who, space.instance, space.state)
  }

  def canWrite(who: User)(implicit space: CommonSpace): Boolean = {
    Conversations.canWriteComments(who.mainIdentity.id, space.instance, space.state)
  }

  "A Space" should {
    "get the default Conversation permissions right" in {
      implicit val s = commonSpace
      assert(canRead(owner))
      assert(canRead(member))
      assert(canRead(nonMember))

      assert(canWrite(owner))
      assert(canWrite(member))
      assert(!canWrite(nonMember))
    }

    "be able to prevent anyone from commenting" in {
      class TSpace extends CommonSpace {
        override def otherSpaceProps = Seq(instancePermissions(Conversations.CanComment(AccessControl.OwnerTag)))
      }
      implicit val s = new TSpace

      assert(canRead(owner))
      assert(canRead(member))
      assert(canRead(nonMember))

      assert(canWrite(owner))
      assert(!canWrite(member))
      assert(!canWrite(nonMember))
    }

    "be able to prevent non-Members from reading Comments" in {
      class TSpace extends CommonSpace {
        override def otherSpaceProps = Seq(instancePermissions(Conversations.CanReadComments(AccessControl.MembersTag)))
      }
      implicit val s = new TSpace

      assert(canRead(owner))
      assert(canRead(member))
      assert(!canRead(nonMember))

      assert(canWrite(owner))
      assert(canWrite(member))
      assert(!canWrite(nonMember))
    }
  }
}
