package querki.display

import org.scalajs.dom.{raw => dom}
import scalatags.JsDom.all._
import org.querki.gadgets._

import querki.globals._

import querki.comm.URL
import querki.pages._

/**
 * Encapsulates the notion of a "paginated" page, where you can navigate through multiple
 * instances. Wraps around Bootstrap's Paginator widget, to paginate things at the page level.
 *
 * Why "PagePaginator"? Because I expect to wind up wanting a sub-page version of this as well,
 * that changes the values without changing the URL.
 *
 * This is currently very hard-cody. In the medium term, we should think about exposing this
 * capability to Space owners, to use as they see fit. It seems likely that larger Spaces will
 * often want the ability to paginate. That suggests that this may become a hookable Gadget,
 * similar to InputGadgets.
 */
class PagePaginator(
  nElements: Int,
  urlConstr: ParamMap => URL,
  params: ParamMap
)(implicit
  val ecology: Ecology
) extends Gadget[dom.HTMLDivElement]
     with EcologyMember {

  lazy val currentPage = params.get("page").map(java.lang.Integer.parseInt(_)).getOrElse(1)
  lazy val pageSize = params.get("pageSize").map(java.lang.Integer.parseInt(_)).getOrElse(10)

  def pageUrl(pageNum: Int) = urlConstr(params + ("page" -> pageNum.toString))

  def doRender() = {
    // Suggested on this page: http://stackoverflow.com/questions/17944/how-to-round-up-the-result-of-integer-division
    val totalPages = (nElements - 1) / pageSize + 1
    if (totalPages > 1) {
      val leftPage = Math.max(currentPage - 3, 0) + 1
      val rightPage = Math.min(currentPage + 3, totalPages) + 1
      val range = leftPage until rightPage

      div(
        ul(
          cls := "pagination pagination-centered",
          li(a(href := pageUrl(1), raw("&laquo;"))),
          if (currentPage == 1)
            li(cls := "disabled", a(raw("&lsaquo;")))
          else
            li(a(href := pageUrl(currentPage - 1), raw("&lsaquo;"))),
          if (leftPage > 1)
            li(cls := "disabled", a("...")),
          range.map { pageNum =>
            li(cls := { if (pageNum == currentPage) "active" else "" }, a(href := pageUrl(pageNum), pageNum.toString))
          },
          if (rightPage <= totalPages)
            li(cls := "disabled", a("...")),
          if (currentPage == totalPages)
            li(cls := "disabled", a(raw("&rsaquo;")))
          else
            li(a(href := pageUrl(currentPage + 1), raw("&rsaquo;"))),
          li(a(href := pageUrl(totalPages), raw("&raquo;")))
        )
      )
    } else
      div()
  }
}
