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
  
  println(s"Creating a new RatingGadget $this")
  
  def rating = $(elem)
    
  def hook() = {    
    // This is an ugly way to build up the Options, but other alternatives have failed:
    var options = RatyOptions.
      path("/assets/img").
      click({ (e:dom.Element, current:Int, evt:JQueryEventObject) =>
        values = List(current.toString)
        save()
      })
      
    println(s"Options start as $options")
    
    def addData[T](name:String, transform:(T) => RatyOptions.type) = {
      options = rating.data(name).asInstanceOf[UndefOr[T]].map{v => println(s"Found $name = $v"); transform(v)}.getOrElse(options)
    }

    addData[Int]("rating", { v => options.score(v) })
    addData[String]("labels", { v =>
      val labels = v.split(",")
      println(s"Labels are ${labels.mkString(";")}")
      options.hints(labels).number(labels.length) 
    })
    addData[Boolean]("readonly", { v => options.readOnly(v) })
    addData[String]("target", { v => options.target(s"#$v") })
    addData[Boolean]("targetkeep", { v => options.targetKeep(v) })
    
    println(s"options are $options")
    
    rating.raty(options)
  }
  
  def doRender() = ???
}

object RatingGadget {
  def apply(rawElement:dom.Element)(implicit e:Ecology) = {
    (new RatingGadget).setElem(rawElement)
  }  
}
