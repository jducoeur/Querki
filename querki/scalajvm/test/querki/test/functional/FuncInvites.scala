package querki.test.functional

/**
 * @author jducoeur
 */
trait FuncInvites { self:FuncMixin =>
  def openSharing()(state:State):State = {
    spew("Going into Sharing")
    val space = state.currentSpace.get
    clickMenuItem(SharingItem)
    val page = SharingPage(space)
    waitFor(page)
    state -> page
  }
  
  def shareByEmail(user:TestUser)(state:State):State = {
    run(state,
      openSharing(),
      { state =>
        
        click on textField("invitees")
        pressKeys(user.email + "\t")
        
        eventually { 
          // TODO: this currently only works if there is only one mf_item -- which means only one
          // email or collaborator listed:
          find(className("mf_item")).get.text should include (user.email) 
        }
        click on "_inviteButton"
        waitFor("_alertMsg")
        find(id("_alertMsg")).get.text should include ("Sent invites to")
        click on "_spaceLink"
        waitForTitle(RootPage(state.currentSpace.get))
        
        state
      }
    )
  }
  
  val inviteLinkRegex = """<b><a href=\"([^\"]*)\">Click here</a></b> to accept the invitation.""".r.unanchored
  
  /**
   * Go into the most recent email message sent, and get the invitation link from it.
   */
  def extractInviteLink():String = {
    val session = IEmailInspector.sessions.head
    val email = session.messages.head
    val body = email.bodyMain.strip.toString
    body match {
      case inviteLinkRegex(url) => url
      case _ => throw new Exception(s"Didn't find invitation link in $body")
    }
  }
}
