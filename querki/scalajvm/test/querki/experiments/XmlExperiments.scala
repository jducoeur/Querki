package querki.experiments

import scala.xml.TopScope
import scala.xml.parsing.XhtmlParser

import querki.test._

class XmlExperiments extends QuerkiTests {
  "XhtmlNodeParser" should {
    "be able to parse multiple nodes" in {
      val html = "<span>Hello</span> <span>there</span>"
        
      val parser = new XhtmlParser(scala.io.Source.fromString(html)).initialize
//      parser.nextch
//      println("------> " + parser.ch)
      if (parser.ch == '<') {
        parser.nextch
        val children = parser.element1(TopScope)
      
        println("----> " + children)
      
        // Skip the space:
        parser.nextch
      
        parser.nextch
        val next = parser.element1(TopScope)
        println("----> " + next)
      }
    }
  }
}