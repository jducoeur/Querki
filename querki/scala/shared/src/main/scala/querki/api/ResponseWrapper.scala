package querki.api

import querki.data.UserInfo

/**
 * The structure we send back from every API response. This is *mostly* just the uPickled
 * response to the call, but also includes some standard metadata.
 *
 * @param currentUser The User who is currently logged in here. This seems a little unintuitive --
 *   isn't it simply the user who sent the request? But the reality is that the *cookies* might
 *   have changed out from under you, in a different browser window, that this window knows nothing
 *   about. So we want to make sure that we update the UI. (NOTE: this is a bit inefficient, so this
 *   protocol might change. For example, we might went back just the UserId each time, and have the
 *   client request the full info if it sees that it is out of date. Or we might make the Client detect
 *   the current Cookie state automatically.)
 * @param payload The actual response from the API call that we're processing, as a uPickled string.
 *
 * @author jducoeur
 */
case class ResponseWrapper(
  currentUser: Option[UserInfo],
  payload: String
)
