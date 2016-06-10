package querki.display

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

/**
 * This receives a DList from the server, describing the histogram data, and transforms
 * it into a graphical histogram.
 */
class HistogramGadget(implicit e:Ecology) extends HookedGadget[dom.HTMLTableElement](e) {
  def doRender() = ???
  
  def hook() = {
    // The original DList, which we are going to replace:
    val dl = $(elem)
    val maxWidth:Float = 100  // TODO: can we take this from the width or something?
    
    val labels = dl.find("dt").mapElems($(_).text())
    val scores = dl.find("dd").mapElems(e => Integer.parseInt($(e).text()))
    val maxScore =
      // Seq[T].max apparently does *not* automatically use the Zero if empty; rather, it
      // would throw an Exception. So we need to guard against that.
      if (scores.isEmpty)
        0
      else
        scores.max
    val scale =
      if (maxScore == 0)
        0
      else
        maxWidth / maxScore
        
    val pairs = labels.zip(scores)
    val indexedPairs = pairs.zipWithIndex
    val tbl =
      table(cls:="generatedHistogram",
        indexedPairs.map { indexedPair =>
          val (pair, index) = indexedPair
          tr(
            td(cls:="histoName", pair._1),
            td(cls:="histoScore", pair._2),
            td(div(cls:=s"histoBar histoBar-$index", width:=s"${pair._2 * scale}px"))
          )
        }
      ).render
      
    dl.after(tbl)
    setElem(tbl)
    dl.hide()
  }
}
