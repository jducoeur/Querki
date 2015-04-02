package querki.admin

import scalatags.JsDom.all._

import querki.globals._
import querki.pages._

class StatisticsPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  
  def pageContent =
    Future.successful(PageContents("Current Querki Statistics", div()))
}
