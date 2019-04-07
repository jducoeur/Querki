package querki.spaces

import models._
import Kind.Kind
import querki.basic.MOIDs.SimpleThingOID
import querki.core.MOIDs.{UrPropOID, UrTypeOID}
import querki.globals._
import querki.identity.User
import querki.identity.UserLevel._
import querki.spaces.messages._
import querki.test._
import querki.types.ModelTypeDefiner
import querki.values.QValue

/**
 * This is the stand-in for an actual Property, from outside the black box -- this is used to make
 * the Property useful in tests.
 */
case class PropRecord[VT, RT](oid:OID, cType:Collection, pType:PType[VT] with PTypeBuilder[VT, RT]) {
  def apply(raws:RT*) = (oid, QValue.make(cType, pType, raws:_*))
}

/**
 * This is the SpaceCore-based equivalent of CommonSpace. It has essentially the same elements, but
 * builds them dynamically instead of simply declaring them.
 */
class CommonCoreSpace(implicit e:Ecology) extends SpaceCoreSpace()(e) with ModelTypeDefiner {
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Links = interface[querki.links.Links]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val ExternalLinkType = Links.URLType
  lazy val TextType = Core.TextType
  lazy val LinkType = Core.LinkType
  lazy val TagType = Tags.NewTagSetType
  
  // Boot the Space up
  this ! InitialState(owner, sc.id, "Test Space", owner.mainIdentity.id)
  
  // Build the contents of CommonSpace in it
  // First, create all the Properties...
  val singleLinkProp = addProperty(LinkType, ExactlyOne, "Single Link")
  val optLinkProp = addProperty(LinkType, Optional, "Optional Link")
  val listLinksProp = addProperty(LinkType, QList, "My List of Links")
  val setLinksProp = addProperty(LinkType, QSet, "My Set of Links")
  
  val singleTagProp = addProperty(TagType, ExactlyOne, "Single Tag")
  val optTagProp = addProperty(TagType, Optional, "Optional Tag")
  val listTagsProp = addProperty(TagType, QList, "My List of Tags")
  val setTagsProp = addProperty(TagType, QSet, "My Set of Tags")
  
  val listURLProp = addProperty(ExternalLinkType, QList, "My List of URLs")
  val optURLProp = addProperty(ExternalLinkType, Optional, "My Optional URL")
  
  val singleTextProp = addProperty(TextType, ExactlyOne, "Single Text")
  val optTextProp = addProperty(TextType, Optional, "My Optional Text")
  
  // Then the Models and Things...
  val testModelId = addSimpleModel("My Model")
  val instance = addThing("My Instance", testModelId, optTextProp("Hello world"))
  val withDisplayName = addSimpleThing("Interesting Display Name", Basic.DisplayNameProp("""My name is "interesting"!"""))
  val trivialThing = addSimpleThing("Trivial")  
  
  val nonMember:User = userAs("Non-Member", "nonMemberHandle", PaidUser)
}

class CommonCoreTests extends QuerkiTests {
  "CommonCoreSpace" should {  
    "pass some common tests" in {
      implicit val s = new CommonCoreSpace

      pql("[[My Instance -> My Optional Text]]") should equal ("Hello world")
      
      pql("[[My Model -> _kind]]") should equal ("0")
      pql("[[Text Type -> _kind]]") should equal ("1")
      pql("[[My List of Links._self -> _kind]]") should equal ("2")
      pql("[[Test Space -> _kind]]") should equal ("3")
      pql("[[Optional -> _kind]]") should equal ("4")
      
      pql("""[[My Instance -> _model -> _oid]]""") should equal(s.testModelId.toThingId.toString)
      pql("""[[My Model -> _model -> _oid]]""") should equal(SimpleThingOID.toThingId.toString)
    }
    
    "let me ChangeProps on a Thing" in {
      implicit val s = new CommonCoreSpace
      
      pql("[[My Instance -> My Optional Text]]") should equal ("Hello world")
      s.changeThing(s.instance, s.optTextProp("Hello there"))
      pql("[[My Instance -> My Optional Text]]") should equal ("Hello there")      
    }
    
    "let me DeleteThing" in {
      implicit val s = new CommonCoreSpace
      
      pql("[[My Instance -> My Optional Text]]") should equal ("Hello world")
      s ! DeleteThing(s.owner, s.sc.id, s.instance)
      pql("[[My Instance -> My Optional Text]]") should equal ("")
      pql("[[My Instance]]") should equal (unknownName("My Instance"))

    }
  }
  
  "ReplayCoreSpace" should {
    "allow new data after replay" in {
      val original = new CommonCoreSpace
      implicit val s = new ReplayCoreSpace(original)
      
      val thingy = s.addSimpleThing("New Thingy")
      pql("[[New Thingy]]") should equal ("[New Thingy](New-Thingy)")
    }
  }
}
