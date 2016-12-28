package org.querki.squery

import org.scalajs.dom
import dom.Element
import dom.ext._

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
   * Given a collection of A, this returns any of them or their descendants that match
   * the predicate.
   * 
   * The semantics of the "search" depend on the typeclass instance, but are
   * generally depth-first.
   * 
   * Note that, unlike findAll(), the result set may include the inputs.
   */
  def findInclusive(as:Seq[A], pred:Element => Boolean):Seq[Element]
  
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
  implicit val FindableElement = new Findable[Element] {
    def findAll(a:Element, pred:Element => Boolean):Seq[Element] = {
      findInclusive(a.children, pred)
    }
    
    def findInclusive(as:Seq[Element], pred:Element => Boolean):Seq[Element] = {
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
  
  implicit class FindableElementEasy(e:Element) {
    def findAll(pred:Element => Boolean):Seq[Element] = FindableElement.findAll(e, pred)
    def findFirst(pred:Element => Boolean):Option[Element] = FindableElement.findFirst(e, pred)
  }
}
