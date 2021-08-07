package querki.photos

import scala.scalajs.js
import js.Math
import org.scalajs.dom.{raw => domRaw, _}
import org.querki.jquery._
import scalatags.JsDom.all.{html => htmlTag, _}
import querki.globals._
import querki.util.ScalatagUtils

/**
 * This pseudo-Gadget takes a List or Set of Photos, and turns them into a Bootstrap Carousel.
 *
 * Note that this takes a very simple received div from the server, and massively mutates it
 * to make it Bootstrappy.
 */
object PhotoList extends ScalatagUtils {

  def hook(e: Element) = {
    // Remove the images; we'll re-attach them in a minute:
    val images = $(e).find(".item").get().asInstanceOf[js.Array[Element]]
    $(e).find(".item").detach()

    // A randomly-chosen id to use for the new carousel
    val nodeId = s"carousel-${(Math.random() * Int.MaxValue).toInt}"

    def arrow(
      direction: String,
      slide: String,
      slideHover: String
    ) = {
      a(
        cls := s"$direction carousel-control",
        href := s"#$nodeId",
        role := "button",
        data("slide") := slide,
        span(cls := s"glyphicon glyphicon-chevron-$direction", aria.hidden := "true"),
        span(cls := "sr-only", slideHover)
      )
    }

    val multiple = images.length > 1

    // The body of the new carousel. See
    //   http://getbootstrap.com/javascript/#carousel
    // for the example this is adapted from. Note that we intentionally
    // do *not* make this auto-animate, based on feedback in this poll:
    //   http://jducoeur.livejournal.com/839178.html
    val newDivTags =
      div(
        id := nodeId,
        cls := "carousel slide",
        data("interval") := false,
        // The indicators that you click on to choose a slide:
        if (multiple) {
          ol(
            cls := "carousel-indicators",
            for (n <- 0 to (images.length - 1))
              yield li(
                data("target") := s"#$nodeId",
                data("slide-to") := n,
                if (n == 0) cls := "active"
              )
          )
        },
        // The slides themselves. These will get filled in below:
        div(cls := "carousel-inner", role := "listbox"),
        if (multiple) {
          MSeq(
            arrow("left", "prev", "Previous"),
            arrow("right", "next", "Next")
          )
        }
      )
    val newDiv = newDivTags.render

    // Drop the new representation in place of the old one:
    $(newDiv).insertBefore(e)
    $(e).detach()

    // Stuff the photos into the carousel. This is a bit roundabout because Scalatags has no
    // way to include an existing Element:
    val innards = $(newDiv).find(".carousel-inner")
    for ((wrapper, index) <- images.zipWithIndex) {
      if (index == 0)
        $(wrapper).addClass("active")
      innards.append(wrapper)
    }
  }
}
