package querki.system

import scala.util.Try

import controllers.{PageEventManager, PlayRequestContext}

import querki.ecology._
import querki.identity.User
import querki.util._

object MOIDs extends EcotIds(8)

/**
 * Terms of Service manager. This is a Module mostly for lifecycle management, to plug into the
 * PageEventManager, but it doesn't expose anything to the System Space.
 * 
 * Note that this works hand-in-glove with controllers.TOSController. Really, I would just do the
 * control stuff here, but wound up having strange problems with imports.
 */
class TOSModule(e:Ecology) extends QuerkiEcot(e) with TermsOfService {
  
  import TOSModule._
  
  val PageEventManager = initRequires[controllers.PageEventManager]
  
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  override def init = {
    PageEventManager.requestReceived += TOSChecker
  }
  
  override def term = {
    PageEventManager.requestReceived -= TOSChecker
  }
  
  /**
   * Record that the User has accepted the TOS. May throw an exception, so use inside Tryer!
   */
  def recordAccept(user:User, version:Int):User =  {
    UserAccess.setTOSVersion(user.id, version).get
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
      val newRCOpt = for (
        user <- rc.requester;
        if (user.tosVersion != noTOSUserVersion);
        if (user.tosVersion != currentVersion.version)
          )
        yield rc.copy(sessionUpdates = rc.sessionUpdates ++ rc.returnToHereUpdate, redirectTo = Some(controllers.routes.TOSController.showTOS))
        
      newRCOpt.getOrElse(rc)
    }
  }
}

import models.{DisplayText, Wikitext}

object TOSModule {
  val currentVersion = TOS1
  // Marker for Users who don't need to sign the TOS -- pseudo-users, generally:
  val noTOSUserVersion = -1
  
  /**
   * The current Terms of Service, rendered as HTML.
   */
  def current:DisplayText = Wikitext(currentVersion.text).display
}

case class TOSVersion(version:Int, text:String)

object TOS1 extends TOSVersion(1, """
Draft Terms of Service for Querki
=================================

Welcome to Querki! These Terms of Service govern your access to and use of the service.
By using or accessing Querki and the Spaces, Things and Pages contained in it, you agree to
these Terms, which will be periodically updated as the service evolves. When these Terms are
updated, you will be notified the next time you log into Querki.

Querki is used to manage information ("Things" or "Pages"), which are contained in "Spaces".
Each Space has a single Owner, and may be shared with other Users as that Owner sees fit.
Responsibility for the management of a Space, or management of particular aspects of a Space,
may be delegated to other Users by its Owner. However, the Owner retains final control of,
and responsibility for, their Spaces.

Querki is a rapidly-evolving system, and the services provided by it may change from time
to time without notice to you.

### Intellectual Property

You retain ownership of the information that you enter into Spaces you own in Querki, and
may modify or delete that information as you see fit.

Information entered into Spaces owned by other Users is considered to be under the control
of those users, and they may modify or delete it as they choose. They may or may not also give
you such control over information you enter into their Spaces.

By entering information into Querki, you grant Querki a non-exclusive license to distribute
that information to other people who you designate. If a Space or Thing is designated as
"Public", you grant Querki a license to distribute that information to all persons and
entities whatsoever.

All content of a Space or Thing is the responsibility of that Space's Owner. We may not
monitor or control the content of these Spaces or Things, and cannot take responsibility
for that content. Under no circumstances will Querki be liable for any content in user-created
Spaces, including, but not limited to, any errors or omissions in that content, or any
loss or damage resulting from use of that content.

All content entered by employees or assigns of Querki is copyright by Querki, and all
rights are reserved.

### Public and Private Spaces

Spaces and Things in Querki may be designated as "Public" or "Private".

A Public Space or Thing is considered to be viewable by anyone with access to Querki,
including Web users who are not otherwise Users of Querki. This does not imply that all
of these people can modify your information, merely see it. Querki reserves the right to
display advertising on Public Spaces and Things, which may be targeted based on their
contents; such advertising may depend on the membership status of the Owner and reader,
as well as other factors.

A Private Space or Thing may only be viewed by those Users designated by the Space's Owner.
Querki will not resell or intentionally redistribute for profit the information contained
in Private Spaces or Things, and will not display advertisements on them.

Querki may limit the number of Private Spaces and Things that you own, depending on your
membership status and other factors.

Querki reserves the right to require that certain Spaces and Things be designated as
Private, at the sole discretion of Querki.

### Privacy

Querki makes a good-faith effort to maintain the security and privacy of your personal
information, and of Spaces and Things which you have designated as Private. However,
Querki assumes no liability for disclosure of information designated as Private, and
is not intended for high-security applications.

You are responsible for protecting the password or other credentials that you use to
access Querki, and for any actions performed when using those credentials. We encourage
you to always use strong, difficult-to-guess passwords on Querki and other services,
especially on social networks and other services which may be used to log into Querki.

Querki will cooperate with properly executed subpoenas and warrants from law enforcement,
and assumes no responsibility for information that is disclosed as a result.

### Prohibited Content

While Querki does not monitor the content of Spaces and Things in it, we expect Users
to abide by some general rules of conduct. In particular, illegal and abusive content
or conduct are prohibited, including but not limited to the following:

* You may not use Querki to threaten or plan violence against others.
* You may not use Querki to deliberately harass or intimidate others. In particular,
  you may not circumvent another person's attempts to remove you from a Space which
  they own.
* You may not use Querki to impersonate others in a way that is intended to mislead
  or deceive.
* You may not use Querki to violate copyright or trademark law. We will respond to
  clear and complete notices of alleged copyright infringement, but do not assume
  responsibility to police Querki against such infringements.
* You may not post obscene or pornographic materials in Querki.
* You may not engage in "handle squatting" -- that is, you may not claim a Querki Handle
  for the sole purpose of denying its use by others, or for extorting others who wish
  to use it.
* You may not use Querki for sending any sort of "spam" -- that is, you may not use the service
  to send unsolicited emails. You should only send invitations to people who you know,
  and who you have specific reason to believe will want those invitations. Repeatedly
  sending invitations is considered to be spam, as is using Querki's invitation mechanism
  for sending unsolicited commercial email. We specifically reserve the right to decide
  what constitutes spam.

Violations of these rules may result in suspension or revocation of your account, and/or
suspension or deletion of your Spaces.

### Limitation of Liability

WE TRY TO KEEP QUERKI SECURE, BUG-FREE, AND RUNNING, BUT YOU USE IT AT YOUR OWN RISK.
WE PROVIDE THE SERVICE AS-IS, WITHOUT ANY EXPRESS OR IMPLIED WARRANTEES. WE CAN NOT
GUARANTEE THAT IT WILL BE UP AT ANY GIVEN TIME, NOR THAT PRIVATE INFORMATION CANNOT
BE DISCLOSED.

TO THE EXTENT PERMITTED BY LAW, THE TOTAL LIABILITY OF QUERKI, FOR ANY CLAIMS UNDER
THESE TERMS, IS LIMITED TO THE AMOUNT YOU PAID US TO USE THE SERVICE. IN ALL CASES,
QUERKI WILL NOT BE LIABLE FOR ANY LOSS OR DAMAGE THAT IS NOT REASONABLY FORESEEABLE.
""")