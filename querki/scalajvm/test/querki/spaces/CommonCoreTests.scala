package querki.spaces

import models._
import Kind.Kind
import Thing.emptyProps
import querki.basic.MOIDs.SimpleThingOID
import querki.core.MOIDs.{UrPropOID, UrTypeOID}
import querki.globals._
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
  lazy val Types = interface[querki.types.Types]
  
  lazy val ExternalLinkType = Links.URLType
  lazy val TextType = Core.TextType
  lazy val LinkType = Core.LinkType
  lazy val TagType = Tags.NewTagSetType
  
  /**
   * Use this signature if you need to get a hold of the state after the change is made. Don't
   * over-use this -- there aren't many circumstances where you need it.
   */
  def addSomethingFull(name:String, kind:Kind, model:OID, propList:(OID, QValue)*):Option[AnyRef] = {
    val props = makePropFetcher(name, propList)
    this ! CreateThing(owner, sc.id, kind, model, props)
  }
  
  def addSomething(name:String, kind:Kind, model:OID, propList:(OID, QValue)*):OID = {
    val Some(ThingFound(oid, _)) = addSomethingFull(name, kind, model, propList:_*)
    oid
  }
  
  def addProperty[VT, RT](tpe:PType[VT] with PTypeBuilder[VT, RT], coll:Collection, name:String):PropRecord[VT, RT] = {
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
  
  def addType(name:String, modelId:OID) = {
    val Some(ThingFound(oid, state)) = addSomethingFull(name, Kind.Type, UrTypeOID, Types.ModelForTypeProp(modelId))
    state.typ(oid).asInstanceOf[ModelType]
  }
  
  def changeThing(thingId:OID, propList:(OID, QValue)*) = {
    this ! ChangeProps(owner, sc.id, thingId, Map(propList:_*), true)    
  }
  
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
    }
  }
}
