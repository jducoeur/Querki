package querki.util

import play.api.Logger

/**
 * Mix-in trait that provides logging for this class.
 *
 * This is the standard Querki approach to logging (as of Querki3), and should be used freely.
 *
 * Note that this uses the runtime class to determine the identity of the Logger for config purposes. That is
 * *probably* fine, but we'll see if we decide to lift out a more-flexible variant that lets you specify the
 * class.
 *
 * TODO: consider making use of QLog.inPlay -- if we are *not* in a running Play env (that is, we're in tests),
 * add color to the log output to distinguish the log levels.
 */
trait QLogging {

  /**
   * The standard Play Logger.
   *
   * At least for now, we're designing this as a lazy val so as not to always pay the price of initialization on
   * every object. We'll see if that proves wise or not.
   */
  lazy val logger = Logger(this.getClass)

  def formatMsg(
    msg: => String,
    fileName: sourcecode.FileName,
    line: sourcecode.Line
  ): String = {
    s"(${fileName.value}:${line.value}) $msg"
  }

  def logError(
    msg: => String
  )(implicit
    filename: sourcecode.FileName,
    line: sourcecode.Line
  ): Unit = {
    // Our current thinking is that errors should *always* have a stack trace, by definition:
    stackTrace(msg)
  }

  def stackTrace(
    message: => String
  )(implicit
    filename: sourcecode.FileName,
    line: sourcecode.Line
  ): Unit = {
    try {
      throw new Exception("Debugging Stack Trace requested")
    } catch {
      case ex: Exception => logError(message, ex)
    }
  }

  def logError(
    msg: => String,
    ex: => Throwable
  )(implicit
    filename: sourcecode.FileName,
    line: sourcecode.Line
  ): Unit =
    logger.error(formatMsg(msg, filename, line), ex)

  /**
   * logWarn() should be used for situations that are unexpected not plausible: inconsistencies
   * in User Space that we don't *expect* to see, but could imagine arising under certain
   * circumstances. Basically, stuff to keep an eye on, but which is not immediately alarming.
   *
   * The Option signature here is so that you can say:
   * {{{
   * for {
   *   myThingy <- getThingyOpt orElse logWarn("getThingOpt unexpectedly returned None!")
   * }
   *   ...
   * }}}
   * Basically, it helps with the very common case where you have unexpectedly gotten None inside
   * of an Option for comprehension. (Yes, this is conceptually hackish, but it happens all the time.)
   */
  def logWarn[T](
    msg: => String
  )(implicit
    filename: sourcecode.FileName,
    line: sourcecode.Line
  ): Option[T] = {
    logger.warn(formatMsg(msg, filename, line))

    None
  }

  def logInfo(
    msg: => String
  )(implicit
    filename: sourcecode.FileName,
    line: sourcecode.Line
  ): Unit =
    logger.info(formatMsg(msg, filename, line))

  def logTrace(
    msg: => String
  )(implicit
    filename: sourcecode.FileName,
    line: sourcecode.Line
  ): Unit =
    logger.trace(formatMsg(msg, filename, line))

}
