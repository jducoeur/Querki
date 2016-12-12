package querki.admin

import scalatags.JsDom.all._
import autowire._

import querki.api._
import querki.globals._
import querki.identity.UserLevel._
import querki.pages._

class StatisticsPage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {

  lazy val Client = interface[querki.client.Client]
  
  def pageContent =
    for {
      stats <- Client[AdminFunctions].statistics().call()
      counts = stats.userCountsByLevel.toSeq.sortBy(_._1)
      guts =
        div(
          h1("Current Querki Statistics"),
          h3("User Counts"),
          for (count <- counts)
            yield p(b(levelName(count._1).capitalize, ": "), count._2),
          h3("Spaces"),
          p(b("Total spaces: "), stats.nSpaces)
        )
    }
      yield PageContents("Current Querki Statistics", guts)
}
