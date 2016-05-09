package querki.admin

import scalatags.JsDom.all._
import autowire._

import org.widok.moment._

import querki.api._
import querki.globals._
import querki.pages._
  
import AdminFunctions._

/**
 * @author jducoeur
 */
class MonitorPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  
  def showMember(mem:QMember) = p(mem.address, ": ", mem.status.toString)

  def pageContent = 
    for {
      update <- Client[AdminFunctions].monitor.call()
      guts =
        div(
          h1("Currently Active in Querki"),
          
          h3("Cluster State"),
          p(b("Admin Monitor is running on ", update.monitorNode)),
          p(b("Leader: ", update.state.leader)),
          h4("Members"),
          for (member <- update.state.members)
            showMember(member),
          h4("Unreachable"),
          for (unreach <- update.state.unreachable)
            showMember(unreach),
          
          h3("Active Spaces with User Counts"),
          for {
            space <- update.spaces
          }
            yield p(
              b(space.name), 
              " (on ", space.cluster, ")", 
              ": ", space.nUsers, " users and roughly ", space.size, " bytes",
              " (last updated ", Moment(space.timestamp).calendar(),  ")")
        )
    }
      yield PageContents("Currently Active", guts)
}