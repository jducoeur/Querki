package querki.html

import scala.xml.XML

import models.system.{ExactlyOne, IntType}

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
        equal (expectedWarning("UI.class.htmlRequired"))            
    }
    
    "throw an Exception if no param is given" in {
      implicit val requester = commonSpace.owner
      
      processQText(commonThingAsContext(_.instance), """[[My Optional Text._edit -> _class]]""") should 
        equal (expectedWarning("UI.class.classRequired"))                  
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
  }
}