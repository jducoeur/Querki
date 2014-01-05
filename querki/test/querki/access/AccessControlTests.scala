package querki.access

import org.scalatest.Assertions._

import models.UnknownOID

import models.system.IsModelProp

import querki.ecology._
import querki.identity._
import querki.identity.UserLevel._

import querki.test._

class AccessControlTests extends QuerkiTests {
  lazy val AccessControl = getInterface[querki.security.AccessControl]
  
  "A Space" should {
    "allow the Owner to Edit everything" in {
      val owner = commonSpace.owner
      
      // The Space, implicitly
      assert(AccessControl.canEdit(commonState, owner, UnknownOID))
      // The Space, explicitly
      assert(AccessControl.canEdit(commonState, owner, commonState.id))
      // Model
      assert(AccessControl.canEdit(commonState, owner, commonSpace.testModel.id))
      // Thing
      assert(AccessControl.canEdit(commonState, owner, commonSpace.instance.id))      
    }
    
    "not allow a Member to Edit anything" in {
      val user = commonSpace.member1.user
      
      // The Space, implicitly
      assert(!AccessControl.canEdit(commonState, user, UnknownOID))
      // The Space, explicitly
      assert(!AccessControl.canEdit(commonState, user, commonState.id))
      // Model
      assert(!AccessControl.canEdit(commonState, user, commonSpace.testModel.id))
      // Thing
      assert(!AccessControl.canEdit(commonState, user, commonSpace.instance.id))      
    }
    
    "allow a Member to edit something iff given explicit permission" in {
      class TSpace extends CommonSpace {
        val allowedThing = new SimpleTestThing("Allowed to Members", AccessControl.CanEditProp(member1.person.id))
      }
      val space = new TSpace
      
      assert(AccessControl.canEdit(space.state, space.member1.user, space.allowedThing))
      assert(!AccessControl.canEdit(space.state, space.member2.user, space.allowedThing))      
    }
    
    "allow Members to edit something iff given permission" in {
      class TSpace extends CommonSpace {
        val allowedThing = new SimpleTestThing("Allowed to Members", AccessControl.CanEditProp(AccessControl.MembersTag))
      }
      val space = new TSpace
      
      assert(AccessControl.canEdit(space.state, space.member1.user, space.allowedThing))
      assert(AccessControl.canEdit(space.state, space.member2.user, space.allowedThing))
      assert(!AccessControl.canEdit(space.state, space.nonMember, space.allowedThing))
    }
    
    "allow Edit of a Model's children, but not the Model, with Can Edit Children" in {
      class TSpace extends CommonSpace {
        // A simple default Model and Instance.
        val testModel2 = new SimpleTestThing("My Model", IsModelProp(true), AccessControl.CanEditChildrenProp(member1.person.id))
        val instance2 = new TestThing("My Instance", testModel2) 
      }
      val space = new TSpace

      assert(AccessControl.canEdit(space.state, space.member1.user, space.instance2))
      assert(!AccessControl.canEdit(space.state, space.member2.user, space.instance2))
      assert(!AccessControl.canEdit(space.state, space.nonMember, space.instance2))
      assert(!AccessControl.canEdit(space.state, space.member1.user, space.testModel2))
    }
    
    "allow Edit of a Space's Things, but not the Space, with Can Edit Children" in {
      class TSpace extends CommonSpace {
        override lazy val otherSpaceProps = Seq(AccessControl.CanEditChildrenProp(AccessControl.MembersTag))
      }
      val space = new TSpace

      assert(AccessControl.canEdit(space.state, space.member1.user, space.instance))
      assert(AccessControl.canEdit(space.state, space.member2.user, space.instance))
      assert(!AccessControl.canEdit(space.state, space.nonMember, space.instance))
      assert(!AccessControl.canEdit(space.state, space.member1.user, UnknownOID))
      assert(!AccessControl.canEdit(space.state, space.member1.user, space.state.id))
    }
    
    "allow members to Edit the Space iff given permission" in {
      class TSpace extends CommonSpace {
        override lazy val otherSpaceProps = Seq(AccessControl.CanEditProp(member1.person.id))
      }
      val space = new TSpace

      assert(!AccessControl.canEdit(space.state, space.member1.user, space.instance))
      assert(!AccessControl.canEdit(space.state, space.nonMember, space.instance))
      assert(AccessControl.canEdit(space.state, space.member1.user, UnknownOID))
      assert(AccessControl.canEdit(space.state, space.member1.user, space.state.id))      
      assert(!AccessControl.canEdit(space.state, space.member2.user, UnknownOID))
    }
  }
}
