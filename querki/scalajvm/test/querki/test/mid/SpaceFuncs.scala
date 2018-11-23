package querki.test.mid

import autowire._

import play.api.mvc.Session

import querki.data.SpaceInfo
import querki.globals._
import querki.session.UserFunctions

/**
 * Provides functions for creating and manipulating Spaces.
 */
trait SpaceFuncs { self: ClientFuncs =>
  def createSpace(name: String)(implicit session: Session): SpaceInfo = {
    withNsClient { client =>
      client[UserFunctions].createSpace(name, None).call().waitFor()
    }
  }
}
