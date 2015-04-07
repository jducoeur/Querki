package querki.pages

import scala.concurrent.Future

import querki.globals._

/**
 * This mix-in trait contains implicits for manipulating *Futures* of Pages. The intent is that you use
 * it to describe things to do to the Page after it loads.
 */
trait PageImplicits {
  implicit class PageFutureOps(fut:Future[Page]) {
    def flashing(isError:Boolean, msg:String):Future[Page] = {
      fut.foreach(_.flash(isError, msg))
      fut
    }
  }
}
