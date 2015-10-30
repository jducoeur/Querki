package querki.admin

import scalatags.JsDom.all._
import autowire._

import querki.api._
import querki.globals._
import querki.pages._

/**
 * @author jducoeur
 */
class MonitorPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {

  lazy val Client = interface[querki.client.Client]

  def pageContent = 
    for {
      update <- Client[AdminFunctions].monitor.call()
      guts =
        div(
          h1("Currently Active in Querki"),
          h3("Active Spaces with User Counts"),
          for {
            space <- update.spaces
          }
            yield p(b(space.name), " (on ", space.cluster, ")", ": ", space.nUsers, " users and roughly ", space.size, " bytes")
        )
    }
      yield PageContents("Currently Active", guts)
}