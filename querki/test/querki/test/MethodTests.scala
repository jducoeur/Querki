package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import models.{Thing, ThingState, Wikitext}
import models.Thing._
import models.system.{ExactlyOne, Optional, QList, QSet}
import Optional.QNone
import models.system.{ExternalLinkType, LargeTextType, LinkType, QLText}
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
  
  // === _sort ===
  "_sort" should {
    class testSpace extends CommonSpace {
      val linkTarget = new SimpleTestThing("Link Target")
      val pointer1 = new SimpleTestThing("First Thing")
      val pointer2 = new SimpleTestThing("Another Thing")
      val wrapper = new SimpleTestThing("Wrapper", listLinksProp(linkTarget, pointer1, pointer2))
    }
    
    "sort by name by default" in {
      val space = new testSpace
      import space._
      processQText(thingAsContext[testSpace](space, _.wrapper), """[[My List of Links -> _sort]]""") should
        equal (listOfLinkText(pointer2, pointer1, linkTarget))
    }
    "warn if the sort transform returns empty" in {
      val space = new testSpace
      import space._
      // This is a fairly ridiculous clause, but should produce a decent warning:
      processQText(thingAsContext[testSpace](space, _.wrapper), """[[My List of Links -> _sort(_filter(_isEmpty))]]""") should
        equal (expectedWarning("Methods._sort.illegalEmpty"))
    }
    
    "sort by direct Display Text Properties" in {
      class TSpace extends CommonSpace {
        val sortingProp = new TestProperty(LargeTextType, Optional, "Prop to Sort")
        
        val theModel = new SimpleTestThing("Sorting Model")
        val thing1 = new TestThing("Thing 1", theModel, sortingProp("Kazam!"))
        val thing2 = new TestThing("Thing 2", theModel, sortingProp("Check!"))
        val thing3 = new TestThing("Thing 3", theModel, sortingProp("Wild!"))
        val thing4 = new TestThing("Thing 4", theModel, sortingProp("Floobity!"))
        val thing5 = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.theModel), """[[Sorting Model._instances -> _sort(Prop to Sort) -> ""[[Name]]: [[Prop to Sort]]""]]""") should
        equal ("\nThing 5: Alphabetical!\nThing 2: Check!\nThing 4: Floobity!\nThing 1: Kazam!\nThing 3: Wild!")      
    }
    
    "sort by indirect Display Text Properties" in {
      class TSpace extends CommonSpace {
        val sortingProp = new TestProperty(LargeTextType, Optional, "Prop to Sort")
        
        val theModel = new SimpleTestThing("Sorting Model")
        val thing1 = new TestThing("Thing 1", theModel, sortingProp("Kazam!"))
        val thing2 = new TestThing("Thing 2", theModel, sortingProp("Check!"))
        val thing3 = new TestThing("Thing 3", theModel, sortingProp("Wild!"))
        val thing4 = new TestThing("Thing 4", theModel, sortingProp("Floobity!"))
        val thing5 = new TestThing("Thing 5", theModel, sortingProp("Alphabetical!"))
        
        val refProp = new TestProperty(LinkType, ExactlyOne, "Reference")
        
        val refModel = new SimpleTestThing("Referring Model")
        val ref1 = new TestThing("Reference 1", refModel, refProp(thing1))
        val ref2 = new TestThing("Reference 2", refModel, refProp(thing2))
        val ref3 = new TestThing("Reference 3", refModel, refProp(thing3))
        val ref4 = new TestThing("Reference 4", refModel, refProp(thing4))
        val ref5 = new TestThing("Reference 5", refModel, refProp(thing5))
      }
      val space = new TSpace
      
      processQText(thingAsContext[TSpace](space, _.theModel), """[[Referring Model._instances -> _sort(Reference -> Prop to Sort, ""Unknown"") -> ""[[Name]]: [[Reference -> Prop to Sort]]""]]""") should
        equal ("\nReference 5: Alphabetical!\nReference 2: Check!\nReference 4: Floobity!\nReference 1: Kazam!\nReference 3: Wild!")      
    }
  }
}