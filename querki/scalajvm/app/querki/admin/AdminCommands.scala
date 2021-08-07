package querki.admin

import scala.concurrent.duration._
import akka.pattern._
import akka.util.Timeout

import querki.console.CommandEffectArgs
import querki.console.ConsoleFunctions._
import querki.globals._
import querki.identity.GuestUser
import querki.spaces.messages._
import querki.util.PublicException

/**
 * This mini-Ecot only exists to hold the Commands. It lives under AdminEcot, and does *not* have its own
 * Ecot ID, so it uses the same MOIDs as AdminEcot.
 */
class AdminCommands(e: Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._

  val Console = initRequires[querki.console.Console]
  val Roles = initRequires[querki.security.Roles]

  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  lazy val Person = interface[querki.identity.Person]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val UserAccess = interface[querki.identity.UserAccess]

  implicit val timeout = Timeout(30 seconds)

  lazy val InspectByEmailCmd = Console.defineAdminCommand(
    InspectByEmailCmdOID,
    "Inspect by Email",
    "This doesn't actually *do* anything -- it just demonstrates that the Console is working."
  ) { args =>
    val inv = args.inv
    implicit val state = inv.state

    val result =
      for {
        emailAddr <- inv.processParamFirstAs(0, TextType)
        targetUser <-
          inv.opt(
            UserAccess.getUserByHandleOrEmail(emailAddr.text)
              .orElse(UserAccess.getIdentityByEmail(emailAddr.text).map(GuestUser(_))),
            Some(PublicException.raw("No User found with that Email Address"))
          )
        identityStrs = targetUser.identities.map(ident =>
          s"Identity ${ident.id}: ${ident.handle} -- ${ident.name} (${ident.email})"
        ).mkString("\n")
        localIdents = Person.localIdentities(targetUser)
        personOpt =
          if (localIdents.isEmpty)
            Person.localPersonIncludingInvitees(targetUser.mainIdentity.id)
          else
            localIdents.headOption.flatMap(Person.localPerson(_))
        MySpaces(ownedByMe, memberOf) <- inv.fut(SpaceOps.spaceManager ? ListMySpaces(targetUser.id))
        ownsMsg =
          if (ownedByMe.isEmpty) "Owns no Spaces"
          else s"Owns Spaces ${ownedByMe.map(det => s"${det.display} (${det.id})").mkString(", ")}"
        memberMsg =
          if (memberOf.isEmpty) "In no Spaces"
          else s"In Spaces ${memberOf.map(det => s"${det.display} (${det.id})").mkString(", ")}"
      } yield DisplayTextResult(
        s"""That is user ${targetUser.id}: ${targetUser.name}
           |$identityStrs
           |
           |$ownsMsg
           |$memberMsg
           |
           |Locally in Space ${state.displayName}:
           |${personOpt.map(QLog.renderThing(_)(inv.state)).getOrElse("")}""".stripMargin
      )

    result.get.map(_.headOption.getOrElse(ErrorResult(s"Couldn't find that email address")))
  }

  lazy val DeleteEmailAddressCmd = Console.defineAdminCommand(
    DeleteEmailAddressCmdOID,
    "Delete Email Address",
    "Removes the specified email address from Querki. Does *not* delete the Identity. USE ONLY TO FIX BROKEN INVITATIONS."
  ) { args =>
    val inv = args.inv
    implicit val state = inv.state

    val result = for {
      emailAddr <- inv.processParamFirstAs(0, TextType)
      resultingUserOpt <- inv.fut(UserAccess.deleteEmailAddress(emailAddr.text))
    } yield DisplayTextResult(s"Email address $emailAddr has been removed from User ${resultingUserOpt.map(_.id)}")

    result.get.map(_.headOption.getOrElse(ErrorResult(s"Couldn't find that email address")))
  }

  lazy val ShowSpaceIdCmd = Console.defineAdminCommand(
    ShowSpaceIdCmdOID,
    "Show Space Id",
    "Given the owner handle and name of a Space, this shows its OID"
  ) { args =>
    val inv = args.inv
    implicit val state = inv.state

    val result = for {
      ownerHandle <- inv.processParamFirstAs(0, TextType)
      ownerIdentityOpt <- inv.fut(IdentityAccess.getIdentity(ownerHandle.text))
      if (ownerIdentityOpt.isDefined)
      ownerId = ownerIdentityOpt.map(_.id).get
      spaceName <- inv.processParamFirstAs(1, TextType)
      spaceId <- inv.fut(SpaceOps.getSpaceId(ownerId, spaceName.text))
    } yield DisplayTextResult(s"The Space has OID $spaceId")

    result.get.map(_.headOption.getOrElse(ErrorResult(s"Couldn't find an Identity with that handle!")))
  }

  // TODO: this no longer belongs here, since it's no longer restricted to the Admins:
  lazy val ShowThingCmd = Console.defineSpaceCommand(
    ShowThingCmdOID,
    "Show Thing",
    "Shows a full dump of the specified Thing",
    Seq(Roles.CanManageSecurityPerm)
  ) { case CommandEffectArgs(inv, api) =>
    implicit val state = inv.state

    val result = for {
      tid <- inv.processParamFirstAs(0, LinkType)
      t <- inv.opt(state.anything(tid))
    } yield DisplayTextResult(QLog.renderThing(t))

    result.get.map(_.headOption.getOrElse(ErrorResult(s"Couldn't find a Thing with that ID")))
  }

  override lazy val props = Seq(
    DeleteEmailAddressCmd,
    InspectByEmailCmd,
    ShowThingCmd,
    ShowSpaceIdCmd
  )
}
