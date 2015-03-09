package querki.display

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all._

import querki.globals._

import querki.display.input.InputGadget

/**
 * This receives a DList from the server, describing the histogram data, and transforms
 * it into a graphical histogram.
 */
class HistogramGadget(implicit e:Ecology) extends InputGadget[dom.HTMLTableElement](e) {
  def values = ???
  def doRender() = ???
  
  def hook() = {
    // The original DList, which we are going to replace:
    val dl = $(elem)
    val maxWidth:Float = 100  // TODO: can we take this from the width or something?
    
    val labels = dl.find("dt").mapElems($(_).text())
    val scores = dl.find("dd").mapElems(e => Integer.parseInt($(e).text()))
    val maxScore = scores.max
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
