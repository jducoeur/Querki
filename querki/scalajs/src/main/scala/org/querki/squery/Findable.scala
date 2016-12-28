package org.querki.squery

import org.scalajs.dom
import dom.Element
import dom.ext._
import dom.raw.HTMLCollection

/**
 * The Findable typeclass represents the ability to "find" within a given entity.
 */
trait Findable[A] {
  /**
   * This does a potentially recursive search through the descendants of a, and returns
   * all the Elements that fit the predicate.
   * 
   * Conceptually, this function corresponds to jQuery's $(element).find(selector) function.
   * In particular, note that root Element a is *not* included in the result set.
   * 
   * TBD: can/should the Elements in this signature be generalized?
   */
  def findAll(a:A, pred:Element => Boolean):Seq[Element]
  
  /**
   * Similar to findAll(), but will include the given element in the possible results.
   * This is what findAll() usually recurses into.
   */
  def findInclusive(a:A, pred:Element => Boolean):Seq[Element]
  
  /**
   * This searches through a, and returns the first B found, if any.
   * 
   * In most cases, you will want findAll instead of this.
   * 
   * For the moment, this has a single inefficient implementation. We will probably
   * want to do something cleverer and short-circuiting in the medium term.
   */
  def findFirst(a:A, pred:Element => Boolean):Option[Element] = findAll(a, pred).headOption
}

object Findable {
  implicit val FindableSeq = new Findable[HTMLCollection] {
    def findAll(as:HTMLCollection, pred:Element => Boolean):Seq[Element] = {
      (Seq.empty[Element] /: as) { (results, a) =>
        results ++ findInclusive(a.children, pred)
      }      
    }
    
    def findInclusive(as:HTMLCollection, pred:Element => Boolean):Seq[Element] = {
      (Seq.empty[Element] /: as) { (results, a) =>
        val thisResult = 
          if (pred(a))
            results :+ a
          else
            results
            
        val childResults = findInclusive(a.children, pred)
        
        thisResult ++ childResults
      }
    }    
  }
  
  implicit def FindableElement[E <: Element] = new Findable[E] {
    def findAll(a:E, pred:Element => Boolean):Seq[Element] = {
      FindableSeq.findInclusive(a.children, pred)
    }
    
    def findInclusive(a:E, pred:Element => Boolean):Seq[Element] = {
      val thisResult =
        if (pred(a))
          Seq(a)
        else
          Seq()
          
      thisResult ++ FindableSeq.findInclusive(a.children, pred)
    }
  }
  
  implicit class FindableBuilder[T : Findable](t:T) {
    def findAll(pred:Element => Boolean):Seq[Element] = implicitly[Findable[T]].findAll(t, pred)
    def findFirst(pred:Element => Boolean):Option[Element] = implicitly[Findable[T]].findFirst(t, pred)
  }
}
