package querki.util

import scala.xml._
import scala.xml.parsing.XhtmlParser

object XmlHelpers {
  /**
   * This deals with a pretty common pattern. We're generally passing NodeSeqs around to represent blocks of HTML.
   * *Usually*, this is actually just a single Elem node, but occasionally it's an actual Seq[Elem]. This function
   * figures out which case we have to hand, and applies the given function to that.
   */
  def mapElems(nodes:NodeSeq)(f:Elem => Elem):NodeSeq = {
    nodes match {
      case elem:Elem => f(elem)
      case _ => nodes.flatMap { node => 
        node match {
          case elem:Elem => f(elem)
          case _ => throw new Exception("Got non-Elem element in NodeSeq: " + node)
        }
      }
    }
  }
  
  /**
   * This is a wrapper around XhtmlParser, and deals with its one great weakness: it can't cope with a fragment
   * made up of multiple nodes.
   * 
   * So this copes with our common case: a fragment that is made up of XHTML Elements. Note that anything *outside*
   * the elements will be deliberately ignored, so don't expect miracles here! We explicitly do not preserve Text
   * or CDATAs that fall in here. (We probably could, if we ever decide we care, but that isn't the use case we're
   * worrying about currently.)
   */
  def parseXhtmlFragment(str:String):NodeSeq = {
    val parser = new XhtmlParser(scala.io.Source.fromString(str)).initialize
    
    var nodes = NodeSeq.Empty
    while (!parser.eof) {
      // ... and do the right thing with it:
      if (parser.ch == '<') {
        parser.nextch
        nodes = nodes ++ parser.element1(TopScope)
      } else {
        // Note that, if the current character isn't "<", we just ignore it.
        parser.nextch
      }
    }
    
    nodes
  }

}