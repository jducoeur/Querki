package querki.imexport

import fastparse.all._
import Result._

import querki.test._

import querki.ecology._

/**
 * @author jducoeur
 */
class XMLTests extends QuerkiTests {
  lazy val Imexport = interface[querki.imexport.Imexport]
  
  def checkParse[T](parser:Parser[T], str:String) = {
    val result = parser.parse(str)
    // Why not a match here? Because Scala produces a spurious warning about not being
    // able to deal with the outer type at runtime. Don't know why -- the code works as
    // intended -- but the warning is evil. So we'll fall back to a crude asInstanceOf
    // instead:
    if (result.isInstanceOf[Result.Failure]) fail(result.toString())
    result
  }
  
  "XML Parser" should {
    "handle a plain name" in {
      checkParse(XMLParser.xmlNameP, "attrName")
    }
    
    "handle a name with namespace" in {
      checkParse(XMLParser.xmlNameP, "xmlns:ss")
    }
    
    "handle a simple attribute" in {
      checkParse(XMLParser.xmlAttrP, """foo="bar"""")
    }
    
    "handle a namespace attribute" in {
      checkParse(XMLParser.xmlAttrP, """xmlns:ss="https://www.querki.net/"""")
    }
    
    "handle node without attributes" in {
      checkParse(XMLParser.xmlElementP, """<querki></querki>""")
    }
    
    "handle void node without attributes" in {
      checkParse(XMLParser.xmlElementP, """<querki/>""")
    }
    
    "handle node with attributes" in {
      checkParse(XMLParser.xmlElementP, """<querki xmlns:ss="https://www.querki.net/" xmlns:s1="https://www.querki.net/u/.3y283z1/test-space"></querki>""")
    }
    
    "handle a single node with prelude" in {
      checkParse(XMLParser.xmlP, """<?xml version="1.0" encoding="UTF-8"?><querki xmlns:ss="https://www.querki.net/" xmlns:s1="https://www.querki.net/u/.3y283z1/test-space"></querki>""")
    }
  }

  "XML Imexport" should {
    "do a round trip" in {
      val spaceIn = commonSpace.state
      
      val exporter = new XMLExporter
      val xmlStr = exporter.exportSpace(spaceIn)
      val importer = new XMLImporter(getRc(commonSpace))
      val result = importer.readXML(xmlStr)
    }
  }
}