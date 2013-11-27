package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import models.{Thing, ThingState, Wikitext}
import models.Thing._
import models.system.{ExactlyOne, Optional, QList, QSet}
import Optional.QNone
import models.system.{ExternalLinkType, LinkType, QLText}
import models.system.OIDs.{PageOID}

import ql.QLParser

import querki.values.{QLContext, SpaceState}

class MethodTests extends QuerkiTests
{
  // === _filter ===
  "_filter" should {
    "work with _equals" in {
      class testSpace extends CommonSpace {
        val linkTarget = new SimpleTestThing("Link Target")
        val pointer1 = new SimpleTestThing("Pointer 1", singleLinkProp(linkTarget))
        val pointer2 = new SimpleTestThing("Pointer 2", singleLinkProp(sandbox))
        val wrapper = new SimpleTestThing("Wrapper", listLinksProp(pointer1, pointer2))
      }
      
      processQText(thingAsContext[testSpace](new testSpace, _.wrapper), """[[My List of Links -> _filter(_equals(Single Link, Link Target))]]""") should
        equal ("\n[Pointer 1](Pointer-1)")      
    }
  }
  
  // === _linkButton ===
  "_linkButton" should {
    "work with a Link to Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_linkButton(""hello"")]]""") should 
        equal ("""<a class="btn btn-primary" href="Sandbox">hello</a>""")
    }
    "work with an external URL" in {
      processQText(commonThingAsContext(_.withUrl), """[[My Optional URL -> _linkButton(""hello"")]]""") should
        equal ("""<a class="btn btn-primary" href="http://www.google.com/">hello</a>""")
    }
    "quietly ignore an empty context" in {
      processQText(commonThingAsContext(_.withoutUrl), """[[My Optional URL -> _linkButton(""hello"")]]""") should
        equal ("")      
    }
  }
  
  // === _showLink ===
  "_showLink" should {
    "work with a Link to Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_showLink(""hello"")]]""") should 
        equal ("""[hello](Sandbox)""")
    }
    "work with an external URL" in {
      processQText(commonThingAsContext(_.withUrl), """[[My Optional URL -> _showLink(""hello"")]]""") should
        equal ("""[hello](http://www.google.com/)""")
    }
    "quietly ignore an empty context" in {
      processQText(commonThingAsContext(_.withoutUrl), """[[My Optional URL -> _showLink(""hello"")]]""") should
        equal ("")      
    }
    "work with a list of external URLs" in {
      // Note that a List result will have newlines in the QText, intentionally:
      processQText(commonThingAsContext(_.withUrl), """[[My List of URLs -> _showLink(""hello"")]]""") should
        equal ("\n[hello](http://www.google.com/)\n[hello](http://www.querki.net/)")      
    }
    "work with a list of Links" in {
      class testSpace extends CommonSpace {
        val withLinks = new SimpleTestThing("With Links", listLinksProp(sandbox.id, withUrlOID))
      }
      
      processQText(thingAsContext[testSpace](new testSpace, _.withLinks), """[[My List of Links -> _showLink(Name)]]""") should
        equal ("\n[Sandbox](Sandbox)\n[With URL](With-URL)")
    }
  }
}