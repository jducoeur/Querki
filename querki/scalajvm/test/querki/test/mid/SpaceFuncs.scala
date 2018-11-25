package querki.test.mid

import autowire._

import querki.data.SpaceInfo
import querki.globals._
import querki.session.UserFunctions

/**
 * Provides functions for creating and manipulating Spaces.
 */
trait SpaceFuncs { self: ClientFuncs =>
  def createSpace(name: String): TestOp[SpaceInfo] = 
    TestOp.client { _[UserFunctions].createSpace(name, None).call() }
}
