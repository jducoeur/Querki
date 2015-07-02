package querki.imexport

import fastparse.all._

import XMLParser._

import models._
import Thing._

import querki.time.DateTime
import querki.globals._
import querki.types.ModelTypeDefiner
import querki.values.{RequestContext, SpaceState}

/**
 * Reads in a QuerkiML file, and builds a Space from it.
 * 
 * @author jducoeur
 */
private [imexport] class XMLImporter(rc:RequestContext)(implicit val ecology:Ecology) extends EcologyMember with ModelTypeDefiner {
  
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  import QuerkiML._
  
  implicit class RichElement(elem:XmlElement) {
    def child(tag:QuerkiML.Tag):XmlElement = elem.child(tag.tag)
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
  
  implicit class RichAttr(attr:QuerkiML.Attr) {
    // TODO: for now, we're ignoring the namespace, which means we're not dealing with potential
    // override issues, where the Space and the App have Things of the same name. Once Apps are
    // real, this will become a genuine concern that we'll have to deal with.
    def get(implicit element:XmlElement):String = element.attr(attr.name).v
    def opt(implicit element:XmlElement):Option[String] = element.attrOpt(attr.name).map(_.v)
    def oid(implicit element:XmlElement):OID = importOID(get(element))
    def tid(implicit element:XmlElement):ThingId = importThingId(get(element))
    def prop(implicit element:XmlElement, state:SpaceState) = state.prop(tid).get
    def typ(implicit element:XmlElement, state:SpaceState) = state.typ(tid).get
    def coll(implicit element:XmlElement, state:SpaceState) = state.coll(tid).get
  }
  
  implicit class RichSpace(state:SpaceState) {
    def and(tag:QuerkiML.Tag, builder:(SpaceState, XmlElement) => SpaceState)(implicit node:XmlElement):SpaceState = {
      val section = node.child(tag)
      section.addToSpace(state, builder)
    }
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
  
  def buildType(state:SpaceState, node:XmlElement) = {
    implicit val n = node
    val t = new ModelType(id.oid, modelid.oid, () => emptyProps)
    state.copy(types = state.types + (t.id -> t))
  }
  
  def buildProperty(state:SpaceState, node:XmlElement) = {
    implicit val n = node
    implicit val s = state
    val p =
      Property(
        id.oid,
        state.id,
        modelref.prop.id,
        // HACK: same as in SpaceLoader, still don't know a good way to do this:
        ptyp.typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]],
        coll.coll,
        () => emptyProps, // TODO: fill this in!
        DateTime.now
      )
    state.copy(spaceProps = state.spaceProps + (p.id -> p))
  }

  def readXML(xml:String) = {
    val root = XMLParser.xmlP.parse(xml).get.value
    querki(root) {
      implicit val spaceNode = root.child(space)
      val rawSpace = buildSpace(spaceNode)
      
      val fullSpace = rawSpace.
        and(types, buildType).
        and(spaceProps, buildProperty)
    }
  }
  
  def createFromXML(xml:String) = {
    
  }
}