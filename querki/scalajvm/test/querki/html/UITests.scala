package querki.html

import scala.xml.XML

import models.{Thing, AnyProp}

import querki.test._

class UITests extends QuerkiTests {
  // === _class ===
  "_class method" should {
    "throw an Exception if handed a Number" in {
      class TSpace extends CommonSpace {
        val intProp = new TestProperty(Core.IntType, ExactlyOne, "Int Prop")
        
        val withInt = new SimpleTestThing("Int Thing", intProp(42))
      }
      val space = new TSpace
      
      implicit val requester = commonSpace.owner
      
      processQText(thingAsContext[TSpace](space, _.withInt), """[[Int Prop -> _class(""myClass"")]]""") should 
        equal (expectedWarning("UI.transform.htmlRequired"))            
    }
    
    "throw an Exception if no param is given" in {
      implicit val requester = commonSpace.owner
      
      processQText(commonThingAsContext(_.instance), """[[My Optional Text._edit -> _class]]""") should 
        equal (expectedWarning("UI.transform.classRequired"))                  
    }
    
    "add classes to an _edit" in {
      implicit val requester = commonSpace.owner
      
      val html = processQText(commonThingAsContext(_.instance), """[[My Optional Text._edit -> _class(""myClass otherClass"")]]""")
      val xml = XML.loadString(html)
      val classesOpt = xml.attribute("class")
      assert(classesOpt.isDefined)
      val classes = classesOpt.get
      // Make sure it hasn't lost the original class from _edit:
      assert(classes.toString.contains("propEditor"))
      assert(classes.toString.contains("myClass"))
      assert(classes.toString.contains("otherClass"))
    }
    
    "add classes to a text" in {
      processQText(commonThingAsContext(_.instance), """[[""hello world"" -> _class(""myClass otherClass"")]]""") should
        equal ("""<span class="myClass otherClass">hello world</span>""")
    }
    
    "add classes to a bullet list" in {
      processQText(commonThingAsContext(_.instance), """[[""* hello
          |* world"" -> _class(""myClass otherClass"")]]""".stripMargin) should
        equal ("<ul class=\"myClass otherClass\">\n<li>hello</li>\n<li>world</li>\n</ul>")
    }
    
    "add classes correctly to a multiparagraph text" in {
      processQText(commonThingAsContext(_.instance), """[[""hello
          |
          |world"" -> _class(""myClass otherClass"")]]""".stripMargin) should
        equal ("""<span class="myClass otherClass">hello</span><span class="myClass otherClass">world</span>""")
    }
  }
  
  def testBackLinkWith(cmd:String, parent:Thing, prop:AnyProp, f:Thing => String) = {
    processQText(commonThingAsContext(_ => parent), s"""[[My Model -> ${prop.displayName}.$cmd(""Create"")]]""", Some(parent)) should
      include (s"""v-${prop.id.toString}-=${f(parent)}""")      
  }
    
  // === _createButton ===
  "_createButton" should {
    def testWith(parent:Thing, prop:AnyProp)(implicit f:Thing => String) = testBackLinkWith("_createButton", parent, prop, f)
    
    // Note that these tests are a bit unrealistic: we're using Properties that don't actually exist on the
    // target Model, so they'd fail when you press the button. But the code doesn't currently sanity-check
    // that.
    "work with Links" in {
      implicit val f:Thing => String = _.id.toThingId.toString
      
      testWith(commonSpace.instance, commonSpace.singleLinkProp)
      testWith(commonSpace.instance, commonSpace.optLinkProp)
      testWith(commonSpace.instance, commonSpace.listLinksProp)
      testWith(commonSpace.instance, commonSpace.setLinksProp)
    }
    
    "work with Tags" in {
      implicit val f:Thing => String = _.toThingId.toString
      
      testWith(commonSpace.instance, commonSpace.singleTagProp)
      testWith(commonSpace.instance, commonSpace.optTagProp)
      testWith(commonSpace.instance, commonSpace.listTagsProp)
      testWith(commonSpace.instance, commonSpace.setTagsProp)
    }    
  }
  
  // === _createInstanceLink ===
  "_createInstanceLink" should {
    def testWith(parent:Thing, prop:AnyProp)(implicit f:Thing => String) = testBackLinkWith("_createInstanceLink", parent, prop, f)
    
    // Note that these tests are a bit unrealistic: we're using Properties that don't actually exist on the
    // target Model, so they'd fail when you press the button. But the code doesn't currently sanity-check
    // that.
    "work with Links" in {
      implicit val f:Thing => String = _.id.toThingId.toString
      
      testWith(commonSpace.instance, commonSpace.singleLinkProp)
      testWith(commonSpace.instance, commonSpace.optLinkProp)
      testWith(commonSpace.instance, commonSpace.listLinksProp)
      testWith(commonSpace.instance, commonSpace.setLinksProp)
    }
    
    "work with Tags" in {
      implicit val f:Thing => String = _.toThingId.toString
      
      testWith(commonSpace.instance, commonSpace.singleTagProp)
      testWith(commonSpace.instance, commonSpace.optTagProp)
      testWith(commonSpace.instance, commonSpace.listTagsProp)
      testWith(commonSpace.instance, commonSpace.setTagsProp)
    }    
  }
  
  // === _data ===
  "_data method" should {
    "add a simple data attribute to a span" in {
      processQText(commonThingAsContext(_.instance), """[[""hello world"" -> _data(""foo"",""I am some data"")]]""") should
        equal ("""<span data-foo="I am some data">hello world</span>""")            
    }
    
    "add a simple data attribute to a div" in {
      processQText(commonThingAsContext(_.instance), """[[""{{myClass:
          |hello world
          |}}"" -> _data(""foo"",""I am some data"")]]""".stripMargin) should
        equal ("""<div data-foo="I am some data" class="myClass">
            |<span>hello world</span></div>""".stripReturns)
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
  
  // === _propLink ===
  "_propLink" should {
    "work normally" in {
      implicit val s = new CDSpace
      
      pql("""[[My Favorites -> Show Favorites._propLink -> ""__Faves__""]]""") should
        equal ("[Faves](My-Favorites?prop=Show-Favorites)")
    }
  }
  
  "_QLButton" should {
    "produce the right button" in {
      implicit val s = commonSpace
      
      pql("""[[My Instance -> _QLButton(""Label"", My Optional Text, ""myTarget"")]]""") should
        equal ("""<input type="button" value="Label" class="btn btn-primary _qlInvoke" data-thingid="My-Instance" data-target="myTarget" data-ql="My Optional Text" href="#"></input>""")
    }
  }
  
  // === _section ===
  "_section" should {
    "work normally" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, QList, "Numbers")
        
        val myThing = new SimpleTestThing("Test Thing", numProp(1,2,3,4,5))
      }
      implicit val s = new TSpace
      
      pql("""[[Test Thing -> Numbers -> _section(""Nums: "", ""____"")]]""") should
        equal ("""
			Nums: 
			
			1
			2
			3
			4
			5""".strip)
    }
    
    "work with an empty list" in {
      implicit val s = commonSpace
      pql("""The result is [[Trivial._instances -> _section(""Inner Header"", _bulleted)]]""") should
        equal("""The result is """)
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
  
  // === _tooltip ===
  "_tooltip method" should {
    "add a tooltip to a text block" in {
      processQText(commonThingAsContext(_.instance), """[[""hello world"" -> _tooltip(""I am a tooltip"")]]""") should
        equal ("""<span title="I am a tooltip" class="_withTooltip">hello world</span>""")      
    }
  }
}