package querki.system

import controllers.{Contributor, PageEventManager, PlayRequestContext, Publisher}

/**
 * Terms of Service manager. This is a Module mostly for lifecycle management, to plug into the
 * PageEventManager, but it doesn't expose anything to the System Space.
 */
class TOSModule(val moduleId:Short) extends modules.Module {
  
  override def init = {
    PageEventManager.requestReceived += TOSChecker
  }
  
  override def term = {
    PageEventManager.requestReceived -= TOSChecker
  }
  
  import controllers.PlayRequestContext
  /**
   * This is called via callbacks when we are beginning to render a page. It looks to see whether the
   * URL is an invitation to join this Space, and goes to the Invitation workflow if so.
   * 
   * TODO: this is dependent on PlayRequestContext, which means that it really belongs in controllers!
   */
  object TOSChecker extends Contributor[PlayRequestContext,PlayRequestContext] {
    def notify(rc:PlayRequestContext, sender:Publisher[PlayRequestContext, PlayRequestContext]):PlayRequestContext = {
      rc
//      val rcOpt =
//        for (
//          encodedInvite <- rc.firstQueryParam(inviteParam);
//          spaceId <- rc.spaceIdOpt;
//          ownerHandle <- rc.reqOwnerHandle;
//          hash = SignedHash(encodedInvite, Email.emailSepChar);
//          // TODO: we should do something smarter if this fails:
//          if (Hasher.checkSignature(hash));
//          SignedHash(_, _, msg, _) = hash;
//          Array(personIdStr, emailAddrStr, _*) = msg.split(":");
//          emailAddr = EmailAddress(emailAddrStr);
//          updates = Map((personParam -> personIdStr), (identityEmail -> emailAddrStr))
//        )
//          yield rc.copy(sessionUpdates = rc.sessionUpdates ++ rc.returnToHereUpdate ++ updates,
//              redirectTo = Some(controllers.routes.LoginController.handleInvite(ownerHandle, spaceId)))
//              
//      // This gets picked up in Application.withSpace(), and redirected as necessary.
//      rcOpt.getOrElse(rc)
    }
  }
  

}