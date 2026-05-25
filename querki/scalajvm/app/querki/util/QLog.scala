package querki.util

import models._

import querki.types.ModelTypeBase
import querki.values.SpaceState

object QLog extends QLogging {

  /**
   * Hack: we need to know whether we are in the Play environment or not, and that's
   * no longer statically available. So instead, we just have the unit tests declare
   * when they are running.
   */
  var runningUnitTests: Boolean = false

  def inPlay: Boolean = !runningUnitTests

  def logTraceRet[T](block: => T): T = {
    val ret: T = block
    logTrace(ret.toString)
    ret
  }

  def renderBundle(t: PropertyBundle)(implicit state: SpaceState): String = {
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
      case e: Throwable => "Error fetching properties: " + e
    }
  }

  def renderThing(t: Thing)(implicit state: SpaceState) = {
    def displayName = {
      try {
        t.displayName
      } catch {
        case e: Throwable => "Error fetching displayName: " + e
      }
    }

    def renderProps = renderBundle(t)

    s"""Thing #${t.id} ($displayName)
  In Space #${t.spaceId}
  Model #${t.model}
  Kind: ${t.kind}
  Properties:
$renderProps
"""
  }

  def logTraceThing(t: Thing)(implicit state: SpaceState) = {
    val rendered = renderThing(t)
    logTrace(rendered)
  }

  def logTraceState(state: SpaceState): Unit = {
    logTrace(s"Full details of $state")
    implicit val s = state

    logTrace("Apps")
    state.apps.foreach { app =>
      logTrace("=============================================")
      logTraceState(app)
      logTrace("=============================================")
    }

    logTrace(s"The State Itself")
    logTrace(s"==========")
    logTraceThing(state)

    logTrace(s"Local Things")
    logTrace(s"==========")
    for {
      thing <- state.localThings
    } logTraceThing(thing)

    logTrace(s"Local Props")
    logTrace(s"==========")
    for {
      prop <- state.spaceProps.values
    } logTraceThing(prop)

    val types = state.types.values
    if (!types.isEmpty) {
      logTrace(s"Local Types")
      logTrace(s"==========")
      for {
        tpe <- types
        if (tpe.isInstanceOf[ModelTypeBase])
      } logTrace(s"${tpe.id} (${tpe.displayName}), based on ${tpe.asInstanceOf[ModelTypeBase].basedOn}")
    }
  }
}
