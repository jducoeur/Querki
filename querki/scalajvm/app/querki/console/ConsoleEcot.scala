package querki.console

import scala.xml.NodeSeq

import models._
import querki.api._
import querki.core.QLText
import querki.ecology._
import querki.globals._
import querki.values._

import ConsoleFunctions._

object MOIDs extends EcotIds(71) {
  val SpaceCommandOID = moid(1)
  val CommandTypeOID = moid(2)
  val TestCommandOID = moid(3)
  val AdminCommandOID = moid(4)
  val TestAdminCommandOID = moid(5)
}

class ConsoleEcot(e: Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with Console {

  import MOIDs._

  val AccessControl = initRequires[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val QL = interface[querki.ql.QL]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val System = interface[querki.system.System]
  lazy val TimeProvider = interface[querki.time.TimeProvider]

  override def postInit() = {
    ApiRegistry.registerApiImplFor[ConsoleFunctions, ConsoleFunctionsImpl](SpaceOps.spaceRegion, true, true)
  }

  def invoke(
    context: AutowireApiImpl,
    cmdStr: String
  ): Future[CommandResult] = {
    implicit val state = context match {
      case stateImpl: SpaceApiImpl => stateImpl.state
      case _                       => System.State
    }
    val qlContext = QLContext(ExactlyOne(LinkType(state)), Some(context.rc), TimeProvider.qlEndTime)
    val cmdText = QLText(cmdStr)

    // First, we process the command as QL. Note that we do this completely ignoring permissions
    // (aside from the built-in read permission), but that's okay -- processMethod() is pure.
    QL.processMethod(cmdText, qlContext).flatMap { qv =>
      // The result *should* be a single CommandEffect. If it isn't, something's very wrong.
      // TODO: this needs to become more sophisticated, and cope with errors and such more
      // gracefully:
      val effect = qv.firstAs(CommandType).getOrElse(throw new ConsoleException("That isn't a legal Command!"))

      // Now the important stuff -- checking whether the user is allowed to do this.
      // TODO: make this more composable and FP, instead of throwing Exceptions!

      // Is this an Admin-only command?
      val adminPermOpt = effect.command.firstOpt(AdminCommandProp)
      if (adminPermOpt.isDefined) {
        // Yes -- check whether they're an admin
        if (!context.user.isAdmin) {
          throw new ConsoleException(s"Command ${effect.command} is only legal for Querki administrators.")
        }
      } else {
        // No -- fetch the Command's required Permission
        val permOpt = for {
          permId <- effect.command.firstOpt(SpaceCommandProp)
          rawProp <- state.prop(permId)
          perm <- rawProp.confirmType(LinkType)
        } yield perm
        val perm = permOpt.getOrElse(
          throw new ConsoleException(s"Command ${effect.command} didn't have a proper SpaceCommandProp!")
        )

        // ... and check whether the user *has* that Permission:
        // TODO: this should allow us to check on some object other than state. Indeed, invoke() should really take a
        // ThingId, use that for the QLContext, and use it here.
        if (!AccessControl.hasPermission(perm, state, context.user, state.id))
          // TODO: this should give a more useful error.
          throw new ConsoleException(s"You don't have permission to invoke the command ${effect.command.displayName}!")
      }

      // Finally, we seem to have passed the gauntlet, so it's time to actually do it:
      effect.effect(context)
    }
  }

  def defineAdminCommand(
    oid: OID,
    name: String,
    summary: String,
    otherProps: (OID, QValue)*
  )(
    handler: CommandEffectArgs => Future[CommandResult]
  ) = {
    new InternalMethod(
      oid,
      toProps(
        setName(name),
        setInternal,
        AdminCommandProp(true),
        Summary(summary)
      )
    ) {
      override def qlApply(invIn: Invocation): QFut = {
        fut(ExactlyOne(CommandType(CommandEffect(this, api => handler(CommandEffectArgs(invIn, api))))))
      }
    }
  }

  def defineSpaceCommand(
    oid: OID,
    name: String,
    summary: String,
    perms: Seq[OID],
    otherProps: (OID, QValue)*
  )(
    handler: CommandEffectArgs => Future[CommandResult]
  ) = {
    new InternalMethod(
      oid,
      toProps(
        setName(name),
        setInternal,
        SpaceCommandProp(perms: _*),
        Summary(summary)
      )
    ) {
      override def qlApply(invIn: Invocation): QFut = {
        fut(ExactlyOne(CommandType(CommandEffect(this, api => handler(CommandEffectArgs(invIn, api))))))
      }
    }
  }

  /**
   * *********************************************
   * TYPES
   * *********************************************
   */

  lazy val CommandType = new SystemType[CommandEffect](
    CommandTypeOID,
    toProps(
      setName("_commandType"),
      setInternal,
      Summary("Type used for the return value for Commands. Cannot be instantiated except by Commands.")
    )
  ) with SimplePTypeBuilder[CommandEffect] {
    def doDeserialize(ser: String)(implicit state: SpaceState): CommandEffect = ???
    def doSerialize(v: CommandEffect)(implicit state: SpaceState): String = ???

    def doWikify(
      context: QLContext
    )(
      v: CommandEffect,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ): Future[Wikitext] = {
      fut(Wikitext("That is a Command, and can only be used"))
    }
    def doDefault(implicit state: SpaceState): CommandEffect = ???
    def doComputeMemSize(v: CommandEffect): Int = 0
  }

  override lazy val types = Seq(
    CommandType
  )

  /**
   * *********************************************
   * PROPERTIES
   * *********************************************
   */

  /**
   * For now, Space Commands are required to specify one required Permission.
   *
   * TBD: are there are times when we need to specify more than one Permission? Is it ever okay to
   * *not* specify one? I'd like to err on the side of requiring it, to avoid accidentially
   * leaving a Command too open.
   */
  lazy val SpaceCommandProp = new SystemProperty(
    SpaceCommandOID,
    Core.LinkType,
    ExactlyOne,
    toProps(
      setName("_spaceCommand"),
      setInternal,
      Summary("Commands that can be executed from the Console must set this, and specify the required permissions.")
    )
  )

  /**
   * TBD: ideally, this should have Unit value -- the presence of the flag is the signal, and
   * we don't care about its value.
   */
  lazy val AdminCommandProp = new SystemProperty(
    AdminCommandOID,
    Core.YesNoType,
    Optional,
    toProps(
      setName("_adminCommand"),
      setInternal,
      Summary("Signifies a command that can *only* be executed by a Querki Administrator.")
    )
  )

  lazy val TestCommand = new InternalMethod(
    TestCommandOID,
    toProps(
      setName("_testCommand"),
      setInternal,
      SpaceCommandProp(AccessControl.CanReadProp),
      Summary("This doesn't actually *do* anything -- it just demonstrates that the Console is working.")
    )
  ) {

    override def qlApply(invIn: Invocation): QFut = {
      fut(ExactlyOne(CommandType(CommandEffect(this, api => fut(DisplayTextResult("Console commands are working."))))))
    }
  }

  lazy val TestAdminCommand = defineAdminCommand(
    TestAdminCommandOID,
    "_testAdminCommand",
    "Doesn't do anything, just checks that AdminCommands are working"
  ) { invIn =>
    fut(DisplayTextResult("Admin console commands are working."))
  }

  override lazy val props = Seq(
    SpaceCommandProp,
    AdminCommandProp,
    TestCommand,
    TestAdminCommand
  )
}
