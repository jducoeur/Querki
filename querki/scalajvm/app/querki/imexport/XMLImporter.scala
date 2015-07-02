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
    def mapChildren[T](tag:QuerkiML.Tag)(builder: XmlElement => T):Seq[T] = {
      elem.childrenNamed(tag.tag).map(builder)
    }
    def addToSpace(state:SpaceState, tag:QuerkiML.Tag)(builder: (SpaceState, XmlElement) => SpaceState):SpaceState = {
      (state /: elem.childrenNamed(tag.tag))(builder)
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
    def get(implicit element:XmlElement):String = element.attr(attr.name).v
    def opt(implicit element:XmlElement):Option[String] = element.attrOpt(attr.name).map(_.v)
    def oid(implicit element:XmlElement):OID = importOID(get(element))
  }
  
  def buildSpace(implicit node:XmlElement):SpaceState = {
    SpaceState(
      id.oid,
      systemId,
      () => emptyProps,
      rc.requesterOrAnon.mainIdentity.id,
      name.get,
      DateTime.now,
      None,
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

  def readXML(xml:String) = {
    val root = XMLParser.xmlP.parse(xml).get.value
    querki(root) {
      val spaceNode = root.child(space)
      val rawSpace = buildSpace(spaceNode)
      
      val typeList = spaceNode.child(types).addToSpace(rawSpace, typ)(buildType)
    }
  }
  
  def createFromXML(xml:String) = {
    
  }
}