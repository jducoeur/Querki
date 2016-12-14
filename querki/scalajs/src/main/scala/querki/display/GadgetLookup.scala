package querki.display

import scala.annotation.tailrec

import org.scalajs.dom
import scala.scalajs.js
import js.UndefOr

import org.querki.jquery._

/**
 * These functions deal with mapping from Elements back to their Gadgets.
 */
object GadgetLookup {
  
  def annotateGadget[Output <: dom.Element](g:Gadget[Output]):Unit = {
    g.elemOpt.map { elem =>
      val gadgets =
        if ($(elem).hasClass("_withGadget")) {
          val existingGadgets = $(elem).data("gadgets").asInstanceOf[UndefOr[Seq[AnyFrag]]].getOrElse(Seq.empty)
          existingGadgets :+ g
        } else {
          Seq(g)
        }
      $(elem).data("gadgets", gadgets.asInstanceOf[js.Any])
      $(elem).addClass("_withGadget")
    }
  }
  
  def findGadgetsFor(root:JQuery, pred:AnyFrag => Boolean):Seq[AnyFrag] = {
    val gadgetOptsArray = root.find("._withGadget").map({ (e:dom.Element) =>
      val frags = $(e).data("gadgets").asInstanceOf[Seq[AnyFrag]]
      frags.filter(pred(_))
    }:js.ThisFunction0[dom.Element, Any]).get()
    
    val gadgetOptsSeq:Seq[Seq[AnyFrag]] = gadgetOptsArray.asInstanceOf[js.Array[Seq[AnyFrag]]]
        
    gadgetOptsSeq.flatten
  }
  
  @tailrec private def findParentGadgetRec(node:JQuery, pred:AnyFrag => Boolean):Option[AnyFrag] = {
    if (node.length == 0)
      // Implication is that we've gone all the way to the top of the hierarchy without a match, so
      // give up:
      None
    else {
      val frags = findGadgets(node)
      frags.find(pred(_)) match {
        case Some(result) => Some(result)
        case None => {
          val parent = node.parent()
          if (parent.length > 0 && parent.get(0).get == dom.document)
            None
          else
            findParentGadgetRec(node.parent(), pred)
        }
      }
    }
  }
  def findParentGadget[N <: dom.Node](frag:ManagedFrag[N], pred:AnyFrag => Boolean):Option[AnyFrag] = {
    frag.elemOpt.flatMap(e => findParentGadgetRec($(e), pred))
  }
  
  def findGadgets(node:JQuery):Seq[AnyFrag] = {
    if (node.hasClass("_withGadget"))
      node.data("gadgets").map(_.asInstanceOf[Seq[AnyFrag]]).getOrElse(Seq.empty)
    else
      Seq.empty
  }
  
}