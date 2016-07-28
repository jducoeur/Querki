package querki.spaces

import models._
import Kind.Kind
import querki.basic.MOIDs.SimpleThingOID
import querki.core.MOIDs.UrPropOID
import querki.globals._
import querki.spaces.messages._
import querki.test._
import querki.values.QValue

/**
 * This is the SpaceCore-based equivalent of CommonSpace. It has essentially the same elements, but
 * builds them dynamically instead of simply declaring them.
 */
class CommonCoreSpace(implicit e:Ecology) extends SpaceCoreSpace()(e) {
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Links = interface[querki.links.Links]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val ExternalLinkType = Links.URLType
  lazy val TextType = Core.TextType
  lazy val LinkType = Core.LinkType
  lazy val TagType = Tags.NewTagSetType
  
  def addSomething(name:String, kind:Kind, model:OID, propList:(OID, QValue)*):OID = {
    val props = makePropFetcher(name, propList)
    val Some(ThingFound(oid, _)) = this ! CreateThing(owner, sc.id, kind, model, props)
    oid
  }
  
  def addProperty[VT, RT](name:String, coll:Collection, tpe:PType[VT] with PTypeBuilder[VT, RT]):PropRecord[VT, RT] = {
    val oid = addSomething(name, Kind.Property, UrPropOID,
      Core.CollectionProp(coll),
      Core.TypeProp(tpe)
    )
    PropRecord(oid, coll, tpe)
  }
  
  def addThing(name:String, model:OID, propList:(OID, QValue)*) = {
    addSomething(name, Kind.Thing, model, propList:_*)
  }
  
  def addSimpleThing(name:String, propList:(OID, QValue)*) = {
    addThing(name, SimpleThingOID, propList:_*)
  }
  
  def addSimpleModel(name:String, propList:(OID, QValue)*) = {
    addThing(name, SimpleThingOID,
      (Core.IsModelProp(true) +: propList):_*
    )
  }
  
  // Boot the Space up
  this ! InitialState(owner, sc.id, "Test Space", owner.mainIdentity.id)
  
  // Build the contents of CommonSpace in it
  // First, create all the Properties...
  addProperty("Single Link", ExactlyOne, LinkType)
  addProperty("Optional Link", Optional, LinkType)
  addProperty("My List of Links", QList, LinkType)
  addProperty("My Set of Links", QSet, LinkType)
  
  addProperty("Single Tag", ExactlyOne, TagType)
  addProperty("Optional Tag", Optional, TagType)
  addProperty("My List of Tags", QList, TagType)
  addProperty("My Set of Tags", QSet, TagType)
  
  addProperty("My List of URLs", QList, ExternalLinkType)
  addProperty("My Optional URL", Optional, ExternalLinkType)
  
  addProperty("Single Text", ExactlyOne, TextType)
  val optTextProp = addProperty("My Optional Text", Optional, TextType)
  
  // Then the Models and Things...
  val testModelId = addSimpleModel("My Model")
  val instance = addThing("My Instance", testModelId, optTextProp("Hello world"))
  val withDisplayName = addSimpleThing("Interesting Display Name", Basic.DisplayNameProp("""My name is "interesting"!"""))
  val trivialThing = addSimpleThing("Trivial")  
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
      s ! ChangeProps(s.owner, s.sc.id, s.instance, Map(s.optTextProp("Hello there")), true)
      pql("[[My Instance -> My Optional Text]]") should equal ("Hello there")      
    }
    
    "let me DeleteThing" in {
      implicit val s = new CommonCoreSpace
      
      pql("[[My Instance -> My Optional Text]]") should equal ("Hello world")
      s ! DeleteThing(s.owner, s.sc.id, s.instance)
      pql("[[My Instance -> My Optional Text]]") should equal ("")
    }
  }
}