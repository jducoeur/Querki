package querki.html

import scala.xml.XML

import models.system.{IntType}

import querki.test._

class UITests extends QuerkiTests {
  "_class method" should {
    "throw an Exception if handed a Number" in {
      class TSpace extends CommonSpace {
        val intProp = new TestProperty(IntType, ExactlyOne, "Int Prop")
        
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
    
    "not yet add classes to a multiparagraph text" in {
      processQText(commonThingAsContext(_.instance), """[[""hello
          |
          |world"" -> _class(""myClass otherClass"")]]""".stripMargin) should
        equal (expectedWarning("UI.transform.notWellFormed"))
    }
  }
  
  "_tooltip method" should {
    "add a tooltip to a text block" in {
      processQText(commonThingAsContext(_.instance), """[[""hello world"" -> _tooltip(""I am a tooltip"")]]""") should
        equal ("""<span title="I am a tooltip" class="_withTooltip">hello world</span>""")      
    }
  }
}