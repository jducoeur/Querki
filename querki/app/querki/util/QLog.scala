package querki.util

import play.api.Logger

import models._

object QLog {
  
  def spew(msg:String) = Logger.info("----> " + msg)

  def spewThing(t:Thing) = {
    def displayName = {
      try {
        t.displayName
      } catch {
        case e:Throwable => "Error fetching displayName: " + e
      }
    }
    
    def renderProps = {
      try {
        val props = t.props
        val renderedProps = props.map { pair =>
          val (key, value) = pair
          val elems = value.elems
          val stringifiedElems = elems.map(_.elem.toString).toList.mkString(", ")
          s"    $key: $stringifiedElems"
        }
        renderedProps.mkString("\n")
      } catch {
        case e:Throwable => "Error fetching properties: " + e
      }
    }
    
    val rendered = s"""Thing #${t.id} ($displayName)
  In Space #${t.spaceId}
  Model #${t.model}
  Kind: ${t.kind}
  Properties:
$renderProps
"""
    spew(rendered)
  }
}