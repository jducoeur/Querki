package querki.pages

import scalatags.JsDom.all._

import querki.globals._

/**
 * @author jducoeur
 */
class IndexPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  
  def pageContent =
    Future.successful(PageContents("Index Page", div(b("The list of Spaces will go here"))))
}
