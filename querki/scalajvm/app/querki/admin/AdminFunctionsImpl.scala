package querki.admin

import scala.concurrent.Future

import querki.globals._

import querki.api.AdminFunctions
import AdminFunctions._
import querki.session.{AutowireApiImpl, AutowireParams}

class AdminFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with AdminFunctions {
  
  def statistics:Future[QuerkiStats] = {
    Future.successful(QuerkiStats(0, 0, 0))
  }
  
}
