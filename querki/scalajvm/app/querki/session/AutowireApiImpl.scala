package querki.session

import querki.globals._

import querki.identity.User
import querki.values.{RequestContext, SpaceState}

case class AutowireParams(
  /**
   * The User who is making this request.
   */
  user:User,
  
  /**
   * The current state of the Space, as seen by this User.
   */
  state:SpaceState,
  
  /**
   * The RequestContext for this request.
   */
  rc:RequestContext
)

class AutowireApiImpl(info:AutowireParams, val ecology:Ecology) extends EcologyMember {
  def user = info.user
  def state = info.state
  def rc = info.rc
}
