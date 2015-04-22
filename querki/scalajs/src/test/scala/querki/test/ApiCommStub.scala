package querki.test

import scala.scalajs.js

import querki.globals._

import querki.comm.ApiComm

class ApiCommStub(e:Ecology) extends ClientEcot(e) with ApiComm {
  def implements = Set(classOf[ApiComm])
  
  /**
   * Tests should plug their own entry points into the fields of controllers.
   * 
   * IMPORTANT: all "controllers" used in the tests must be listed here! By and large,
   * all the controllers listed in client.scala.html should be also listed here.
   */
  lazy val controllers = lit(
    Application = lit(),
    ExploreController = lit(),
    AdminController = lit(),
    ClientController = lit(),
    TOSController = lit(),
    LoginController = lit()
  )
  
}
