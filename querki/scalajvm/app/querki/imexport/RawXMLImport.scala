package querki.imexport

import fastparse.all._

import XMLParser._

import models._
import Thing._

import querki.time.DateTime
import querki.globals._
import querki.types.{ModelTypeBase, ModelTypeDefiner, SimplePropertyBundle}
import querki.values.{ElemValue, QValue, RequestContext, SpaceState}

/**
 * Reads in a QuerkiML file, and builds a Space from it.
 * 
 * @author jducoeur
 */
private [imexport] class RawXMLImport(rc:RequestContext)(implicit val ecology:Ecology) extends EcologyMember with ModelTypeDefiner {
  
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  lazy val LinkType = Core.LinkType
  lazy val NameType = Core.NameType
  
  import QuerkiML._
  
  implicit class RichElement(elem:XmlElement) {
    def child(tag:QuerkiML.Tag):XmlElement = elem.child(tag.tag)
    def childOpt(tag:QuerkiML.Tag):Option[XmlElement] = elem.childOpt(tag.tag)
    def childrenNamed(tag:QuerkiML.Tag):Seq[XmlElement] = elem.childrenNamed(tag.tag)
    def addToSpace(state:SpaceState, builder: (SpaceState, XmlElement) => SpaceState):SpaceState = {
      (state /: elem.elements)(builder)
    }
  }
  
  implicit class RichTag(tag:QuerkiML.Tag) {
    def apply[T](element:XmlElement)(builder: => T):T = {
      if (element.tagName.name != tag.tag)
        throw new Exception(s"Failure while reading Querki file -- expected ${tag.tag}, found ${element.tagName.name}")
      
      builder
    }
  }
    
  def nameWithoutSpace(str:String):String = {
    XMLParser.xmlNameP.parse(str).get.value.name
  }
  def importThingId(str:String):ThingId = importOIDOpt(str).map(AsOID(_)).getOrElse(AsName(nameWithoutSpace(str)))
  def parseThingId(str:String):ThingId = importThingId(str)
  
  implicit class RichAttr(attr:QuerkiML.Attr) {
    // TODO: for now, we're ignoring the namespace, which means we're not dealing with potential
    // override issues, where the Space and the App have Things of the same name. Once Apps are
    // real, this will become a genuine concern that we'll have to deal with.
    def get(implicit element:XmlElement):String = element.attr(attr.name).v
    def opt(implicit element:XmlElement):Option[String] = element.attrOpt(attr.name).map(_.v)
    def oid(implicit element:XmlElement):OID = importOID(get(element))
    def oidPlus(implicit element:XmlElement):OID = oidAndName(get(element))
    def tid(implicit element:XmlElement):ThingId = parseThingId(get(element))
    def prop(implicit element:XmlElement, state:SpaceState) = state.prop(tid).get
    def typ(implicit element:XmlElement, state:SpaceState) = state.typ(tid).getOrElse(throw new Exception(s"Didn't find pType $tid, which I got from attribute $attr on $element"))
    def coll(implicit element:XmlElement, state:SpaceState) = state.coll(tid).get
  }
  
  implicit class RichSpace(state:SpaceState) {
    def andChildrenOf(tag:QuerkiML.Tag, builder:(SpaceState, XmlElement) => SpaceState)(implicit node:XmlElement):SpaceState = {
      val section = node.child(tag)
      section.addToSpace(state, builder)
    }
    def andProps(implicit node:XmlElement):SpaceState = {
      state.copy(pf = () => buildProps(state, node))
    }
  }
  
  def oidAndName(str:String):OID = {
    // The OID-and-Name format is "[name] oid"; we care about the oid for this purpose
    val pieces = str.split(' ')
    val oidStr = if (pieces.length > 1) pieces(1) else pieces(0)
    importOID(oidStr)
  }
    
  def buildSpace(implicit node:XmlElement):SpaceState = {
    SpaceState(
      id.oid,
      systemId,
      () => emptyProps,
      rc.requesterOrAnon.mainIdentity.id,
      name.get,
      DateTime.now,
      Some(SystemSpace),
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      None
    )
  }
  
  def buildValue(prop:AnyProp, elem:XmlElement)(implicit state:SpaceState):Option[ElemValue] = {
    def textMap(f:String => ElemValue) = elem.textOpt.map(f)
    
    prop.pType match {
      case LinkType => textMap(s => LinkType(oidAndName(s)))
      
      case mt:ModelTypeBase => {
        elem.childOpt(props).map { bundleProps =>
          val subVals = buildProps(state, elem)
          val bundle = SimplePropertyBundle(subVals)
          mt(bundle)
        }
      }
      
      case NameType => textMap(NameType(_))
      
      case _ => textMap(prop.pType.deserialize(_))
    }
  }
  
  def buildProps(implicit state:SpaceState, node:XmlElement):PropMap = {
    val propSection = node.child(props)
    val propVals = propSection.elements.map { propElem =>
      val prop = state.prop(importThingId(propElem.tagName.name)).getOrElse(throw new Exception(s"Couldn't find property ${propElem.tagName.name}!"))
      val rawElems = propElem.childrenNamed(elem)
      val vs = rawElems.map(buildValue(prop, _)).flatten
      val qv = prop.cType.makePropValue(vs, prop.pType)
      (prop.id -> qv)
    }
    
    Map(propVals:_*)
  }
  
  def buildType(state:SpaceState, node:XmlElement) = {
    implicit val n = node
    implicit val s = state
    val t = new ModelType(id.oid, modelref.oidPlus, () => buildProps)
    state.copy(types = state.types + (t.id -> t))
  }
  
  def buildProperty(state:SpaceState, node:XmlElement) = {
    implicit val n = node
    implicit val s = state
    val p =
      Property(
        id.oid,
        state.id,
        modelref.oidPlus,
        // HACK: same as in SpaceLoader, still don't know a good way to do this:
        ptyp.typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]],
        coll.coll,
        () => buildProps,
        DateTime.now
      )
    state.copy(spaceProps = state.spaceProps + (p.id -> p))
  }
  
  def buildModel(state:SpaceState, node:XmlElement):SpaceState = {
    implicit val n = node
    implicit val s = state
    val model = 
      ThingState(
        id.oid,
        state.id,
        modelref.oidPlus,
        () => buildProps,
        DateTime.now
      )
    state.copy(things = state.things + (model.id -> model))
  }
  
  def buildInstance(state:SpaceState, node:XmlElement) = {
    implicit val n = node
    implicit val s = state
    val model = 
      ThingState(
        id.oid,
        state.id,
        state.anything(parseThingId(node.tagName.name)).get,
        () => buildProps,
        DateTime.now
      )
    state.copy(things = state.things + (model.id -> model))    
  }

  /**
   * Given a full XML export of a Space, this parses that and returns the resulting Space. It
   * does not, in any way, affect the database, though: other code is responsible for actually
   * creating the Space locally. Note that all OIDs in the returned Space should be considered
   * scrap, and should be replaced when instantiating these objects.
   */
  def readXML(xml:String):SpaceState = {
    val root = XMLParser.xmlP.parse(xml).get.value
    querki(root) {
      implicit val spaceNode = root.child(space)
      val rawSpace = buildSpace(spaceNode)
      
      rawSpace.
        andChildrenOf(types, buildType).
        andChildrenOf(spaceProps, buildProperty).
        // Need to get the Space's properties in here, or things tend to break later on:
        andProps.
        andChildrenOf(models, buildModel).
        andChildrenOf(instances, buildInstance)
    }
  }
}