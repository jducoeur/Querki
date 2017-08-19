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
  
  def shareByEmail(users:TestUser*)(state:State):State = {
    run(state,
      openSharing(),
      { state =>
        
        click on textField("invitees")
        users.foreach { user =>
          pressKeys(user.email + "\n")
          eventually { 
            assert(findAll(className("mf_item")).exists(_.text.contains(user.email)))
          }
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
  
  val inviteLinkRegex = 
    """<div class="bottomlinkdiv"><a href="([^\"]*)" class="btn btn-primary">Join Space 'Explore Restricted Space'</a></div>""".r.unanchored
  val validateLinkRegex = """\[([^\"]*)\]""".r.unanchored
  
  def fetchLatestEmailBody():String = {
    val session = IEmailInspector.sessions.head
    val email = session.messages.head
    email.bodyMain.plaintext
  }
  
  def fetchLatestEmailBodyTo(emailAddr:String):String = {
    val sessions = IEmailInspector.sessions.filter(_.messages.exists(_.recipientEmail.addr.equalsIgnoreCase(emailAddr)))
    if (sessions.isEmpty)
      throw new Exception(s"Didn't find any emails targeted at $emailAddr")
    val email = sessions.head.messages.head
    email.bodyMain.plaintext
  }
  
  /**
   * Go into the most recent email message sent, and get the invitation link from it.
   */
  def extractInviteLink(emailAddr:String):String = {
    val body = fetchLatestEmailBodyTo(emailAddr)
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
        
        val inviteLink = extractInviteLink(user.email)
        go to inviteLink
        
        // This takes us to the Space in question, and then we need to go to the Login dialog.
        waitFor(RootPage(space))
        waitFor("_openLoginButton")
        click on "_openLoginButton"
        
        waitFor("_signupButton")
        click on "_signupButton"
        waitFor(SignupPage)
        
        textField("emailInput").value = user.email
        pwdField("passwordInput").value = user.password
        textField("handleInput").value = user.handle
        textField("displayInput").value = user.display
        click on "signupButton"
        
        acceptTermsOfService()(state)
        
        val finalPage = RootPage(state.getSpace(space)) 
        waitFor(finalPage)
        
        state.copy(currentUserOpt = Some(user)) -> finalPage
      }
    )
  }
}
