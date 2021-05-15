package querki.imexport

import fastparse._, NoWhitespace._

import models._

import querki.ecology._
import querki.test._
import querki.types.ComplexSpace
import querki.values.QValue

/**
 * @author jducoeur
 */
class XMLTests extends QuerkiTests with ParserTests {
  lazy val Imexport = interface[querki.imexport.Imexport]

  import XMLParser._

  def checkResult[T](
    parser: P[_] => P[T],
    str: String,
    result: T
  ) = {
    val actual = checkParse(parser, str)
    actual should equal(result)
  }

  def xmlNameW[_ : P] = Wrap(xmlNameP)
  def xmlAttrW[_ : P] = Wrap(xmlAttrP)
  def xmlElementW[_ : P] = Wrap(xmlElementP)
  def xmlW[_ : P] = Wrap(xmlP)
  def xmlEntityW[_ : P] = Wrap(xmlEntityP)
  def xmlTextCharW[_ : P] = Wrap(xmlTextChar)
  def xmlTextW[_ : P] = Wrap(xmlTextP)

  "XML Parser" should {
    "handle a plain name" in {
      checkParse(xmlNameW(_), "attrName")
    }

    "handle a name with namespace" in {
      checkParse(xmlNameW(_), "xmlns:ss")
    }

    "handle a simple attribute" in {
      checkParse(xmlAttrW(_), """foo="bar"""")
    }

    "handle a namespace attribute" in {
      checkParse(xmlAttrW(_), """xmlns:ss="https://www.querki.net/"""")
    }

    "handle node without attributes" in {
      checkParse(xmlElementW(_), """<querki></querki>""")
    }

    "handle void node without attributes" in {
      checkParse(xmlElementW(_), """<querki/>""")
    }

    "handle node with attributes" in {
      checkParse(
        xmlElementW(_),
        """<querki xmlns:ss="https://www.querki.net/" xmlns:s1="https://www.querki.net/u/.3y283z1/test-space"></querki>"""
      )
    }

    "handle a single node with prelude" in {
      checkParse(
        xmlW(_),
        """<?xml version="1.0" encoding="UTF-8"?><querki xmlns:ss="https://www.querki.net/" xmlns:s1="https://www.querki.net/u/.3y283z1/test-space"></querki>"""
      )
    }

    "parse an entity" in {
      checkResult(xmlEntityW(_), "&quot;", '\"')
      checkResult(xmlTextCharW(_), "&quot;", '\"')
    }

    "handle various common entities in text" in {
      checkResult(
        xmlTextW(_),
        """This is some &quot;text&quot;, where anything &lt; Success is going to be &gt; Failure.""",
        XMLParser.XmlText("""This is some "text", where anything < Success is going to be > Failure.""")
      )
    }
  }

  "XML Imexport" should {

    /**
     * This tests that XML import works in its raw form, and produces a Space that matches the export. It
     * does *not* test the actual DB-level Space creation, which wraps around this.
     */
    "do a round trip" in {
      class TestSpace extends ComplexSpace {
        override def otherSpaceProps: Seq[(OID, QValue)] = Seq(optTextProp("I'm a Space!"))
      }

      val theSpace = new TestSpace
      val stateIn = theSpace.state

      val exporter = new XMLExporter
      val xmlStr = exporter.exportSpace(stateIn)
      val importer = new RawXMLImport(getRc(theSpace))
      val stateOut = importer.readXML(xmlStr)

      def pqloaded(text: String) = {
        val rc = getRcs(stateOut)(theSpace, BasicTestUser)
        val context = stateOut.thisAsContext(rc, stateOut, ecology)
        processQText(context, text)
      }

      pqloaded("""[[My Instance -> My Optional Text]]""") should
        equal("""Hello world""")

      // Test that the Space can use its own non-System Property:
      pqloaded("""[[_space -> My Optional Text]]""") should
        equal("""I'm a Space!""")

      // Test Model Types:
      pqloaded("""[[My Complex Thing -> Complex Prop -> Text in Model]]""") should
        equal("Text in Instance")
      pqloaded(
        """[[My Complex Thing -> Top Level Thing -> Meta Property -> _first -> Complex Prop -> Text in Model]]"""
      ) should
        equal("Top Text 1")
      pqloaded("""[[My Tree -> Left -> Right -> Node Id]]""") should
        equal("3")
    }
  }
}
