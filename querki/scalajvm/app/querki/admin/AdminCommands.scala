package querki.admin

import querki.console.ConsoleFunctions._
import querki.globals._

/**
 * This mini-Ecot only exists to hold the Commands. It lives under AdminEcot, and does *not* have its own
 * Ecot ID, so it uses the same MOIDs as AdminEcot.
 */
class AdminCommands(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._
  
  val Console = initRequires[querki.console.Console]
  
  lazy val Person = interface[querki.identity.Person]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  lazy val InspectByEmailCmd = Console.defineAdminCommand(
    InspectByEmailOID, 
    "Inspect by Email",
    "This doesn't actually *do* anything -- it just demonstrates that the Console is working.")
  { inv =>
    
    implicit val state = inv.state
    
    val result = for {
      emailAddr <- inv.processParamFirstAs(0, TextType)
      targetUser <- inv.opt(UserAccess.getUserByHandleOrEmail(emailAddr.text))
      identityStrs = targetUser.identities.map(ident => s"Identity ${ident.id}: ${ident.handle} -- ${ident.name} (${ident.email})").mkString("\n")
      localIdents = Person.localIdentities(targetUser)
      personOpt = localIdents.headOption.flatMap(Person.localPerson(_))
    }
      yield DisplayTextResult(s"""That is user ${targetUser.id}: ${targetUser.name}
                                 |$identityStrs
                                 |${personOpt.map(QLog.renderThing(_)(inv.state)).getOrElse("")}""".stripMargin)
      
    result.get.map(_.headOption.getOrElse(ErrorResult(s"Couldn't find that email address")))
  }

  override lazy val props = Seq(
    InspectByEmailCmd
  )
}
