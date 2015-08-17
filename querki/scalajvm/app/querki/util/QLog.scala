package querki.util

import play.api.{Logger, Play}

import models._

import querki.values.SpaceState

object QLog {
  
  lazy val inPlay:Boolean = Play.maybeApplication.isDefined 
  
  def stackTrace(message: => String) = {
    try {
      throw new Exception("Debugging Stack Trace requested")
    } catch {
      case ex:Exception => error(message, ex)
    }
  }
  
  def error(message: => String, error: => Throwable) = {
    if (inPlay)
      Logger.error(message, error)
    else {
      println(message + "\n" + error.toString())
      error.printStackTrace()
    }
  }
  def error(message: => String) = {
    if (inPlay)
      Logger.error(message)
    else
      println(message)
  }

  /**
   * warn() should be used for situations that are unexpected not plausible: inconsistencies
   * in User Space that we don't *expect* to see, but could imagine arising under certain
   * circumstances. Basically, stuff to keep an eye on, but which is not immediately alarming.
   * 
   * The Option signature here is so that you can say:
   * {{{
   * for {
   *   myThingy <- getThingyOpt orElse QLog.warn("getThingOpt unexpectedly returned None!")
   * }
   *   ...
   * }}}
   * Basically, it helps with the very common case where you have unexpectedly gotten None inside
   * of an Option for comprehension. (Yes, this is conceptually hackish, but it happens all the time.)
   */
  def warn[T](message: => String):Option[T] = {
    if (inPlay)
      Logger.warn(message)
    else
      println(message)
    
    None
  }
  
  def info(message: => String) = {
    if (inPlay)
      Logger.info(message)
    else
      println(message)
  }
  
  def spew(msg:String) = info("----> " + msg)
  
  def spewRet[T](block: => T):T = {
    val ret:T = block
    spew(ret.toString)
    ret
  }
  
  def renderBundle(t:PropertyBundle)(implicit state:SpaceState):String = {
      try {
        val props = t.props
        val renderedProps = props.map { pair =>
          val (key, value) = pair
          val propName = state.prop(key).map(_.displayName).getOrElse(s"??? ($key)")
          val elems = value.elems
          val stringifiedElems = elems.map(_.elem.toString).toList.mkString(", ")
          s"    $propName: $stringifiedElems"
        }
        renderedProps.mkString("\n")
      } catch {
        case e:Throwable => "Error fetching properties: " + e
      }    
  }

  def spewThing(t:Thing)(implicit state:SpaceState) = {
    def displayName = {
      try {
        t.displayName
      } catch {
        case e:Throwable => "Error fetching displayName: " + e
      }
    }
    
    def renderProps = renderBundle(t)
    
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