package querki.console

import querki.api.AutowireApiImpl
import querki.globals._
import querki.ql.Invocation
import querki.util.PublicException

import ConsoleFunctions._

case class CommandEffectArgs(
  inv: Invocation,
  api: AutowireApiImpl
)

/**
 * This is the return value from a Command. It is how Commands can have
 * side-effects: the returned CommandEffect is checked (to make sure that
 * the user has the correct permissions), and then the effect is executed.
 */
case class CommandEffect(
  /**
   * The InternalMethod that resulted in this CommandEffect. Ideally we would just extract that
   * from the parse tree, but that isn't easily available through processMethod(), so we just
   * require that each Command add it in manually.
   *
   * This will be used to check the permissions on the Command, and confirm that this user is
   * allowed to invoke it. We can't check the permissions before executing the Command, but
   * that's okay -- since functions are pure, we don't *actually* care until we need to
   * execute the effect.
   */
  command: AnyProp,
  /**
   * The actual effect. Note that CommandEffect is not serializable because of this! This is
   * allowed and expected to cause some sort of side-effect.
   */
  effect: AutowireApiImpl => Future[CommandResult]
)

case class ConsoleException(msg: String) extends Exception(msg)
