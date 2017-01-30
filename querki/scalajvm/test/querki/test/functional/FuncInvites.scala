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
  val validateLinkRegex = """\[([^\"]*)\]""".r.unanchored
  
  def fetchLatestEmailBody():String = {
    val session = IEmailInspector.sessions.head
    val email = session.messages.head
    email.bodyMain.plaintext
  }
  
  /**
   * Go into the most recent email message sent, and get the invitation link from it.
   */
  def extractInviteLink():String = {
    val body = fetchLatestEmailBody()
    body match {
      case inviteLinkRegex(url) => url
      case _ => throw new Exception(s"Didn't find invitation link in $body")
    }
  }
  
  def extractValidateLink():String = {
    val body = fetchLatestEmailBody()
    body match {
      case validateLinkRegex(url) => url
      case _ => throw new Exception(s"Didn't find validation link in $body")
    }
  }
  
  def acceptTermsOfService()(state:State):State = {
    waitForTitle(TermsOfServicePage)
    checkbox("_TOSagree").select()
    click on "_TOSsubmit"
    state
  }
  
  /**
   * Assumes the most recent email session contains an email to join this Space.
   */
  def acceptInvitationToJoinQuerki(user:TestUser, space:TSpace)(state:State):State = {
    run(state,
      { state =>
        
        val inviteLink = extractInviteLink()
        go to inviteLink
        val handleInvitePage = HandleInvitePage(state.getSpace(space))
        // Note that this isn't part of the client, so we can't use the normal waitFor() --
        // the page will never be "rendered".
        waitForTitle(handleInvitePage)
        
        textField("email").value = user.email
        pwdField("password").value = user.password
        textField("handle").value = user.handle
        textField("display").value = user.display
        click on "_signUpButton"
        
        acceptTermsOfService()(state)
        
        val finalPage = RootPage(state.getSpace(space)) 
        waitFor(finalPage)
        
        state.copy(currentUserOpt = Some(user)) -> finalPage
      }
    )
  }
}
