package querki.display.input

import scala.scalajs.js
import js.UndefOr
import org.scalajs.dom
import org.scalajs.jquery._

import org.scalajs.ext._
import raty._

import querki.globals._

class RatingGadget(implicit e:Ecology) extends InputGadget[dom.HTMLDivElement](e) {
  
  var values = List("0")
  
  def rating = $(elem)
    
  def hook() = {    
    // This is an ugly way to build up the Options, but other alternatives have failed:
    var options = RatyOptions.
      path("/assets/img").
      click({ (e:dom.Element, current:Int, evt:JQueryEventObject) =>
        values = List(current.toString)
        save()
      })
    
    def addData[T](name:String, transform:(T) => RatyOptionBuilder) = {
      options = rating.data(name).asInstanceOf[UndefOr[T]].map{v => transform(v)}.getOrElse(options)
    }

    addData[Int]("rating", { v => options.score(v) })
    addData[String]("labels", { v =>
      val labels = v.split(",")
      options.hints(labels).number(labels.length) 
    })
    addData[Boolean]("readonly", { v => options.readOnly(v) })
    addData[String]("target", { v => options.target(s"#$v") })
    addData[Boolean]("targetkeep", { v => options.targetKeep(v) })
    
    rating.raty(options)
  }
  
  def doRender() = ???
}
