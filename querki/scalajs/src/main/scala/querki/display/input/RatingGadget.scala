package querki.display.input

import scala.scalajs.js
import js.JSConverters._
import js.UndefOr
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._

import org.querki.facades.raty._

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

    // Okay, this is annoying and idiotic. It appears that "data-rating" comes through as either a Number
    // or String, depending on whether it contains a decimal point. Integers get auto-parsed, but Floats don't.
    addData[Any]("rating", { rawV =>
      val v = rawV match {
        case s:String => s.toDouble
        case d:Double => d
      }
      options.score(v) 
    })
    addData[String]("labels", { v =>
      val labels = v.split(",").toJSArray
      options.hints(labels).number(labels.length) 
    })
    addData[Boolean]("readonly", { v => options.readOnly(v) })
    addData[String]("target", { v => options.target(s"#$v") })
    addData[Boolean]("targetkeep", { v => options.targetKeep(v) })
    
    rating.raty(options)
  }
  
  def doRender() = ???
}
