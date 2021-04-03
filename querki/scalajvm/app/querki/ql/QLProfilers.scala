package querki.ql

import querki.ecology.EcologyMember
import querki.globals.Ecology

/**
 * This is created by and obtained from the QLEcot:
 */
private[ql] class QLProfilers(implicit val ecology: Ecology) extends EcologyMember {
  lazy val Profiler = interface[querki.tools.Profiler]

  lazy val parse = Profiler.createHandle("QLParser.parse")
  lazy val processMethod = Profiler.createHandle("QLParser.processMethod")
  lazy val processCall = Profiler.createHandle("QLParser.processCall")
  lazy val processTextStage = Profiler.createHandle("QLParser.processTextStage")
  lazy val processNumber = Profiler.createHandle("QLParser.processNumber")
  lazy val processCallDetail = Profiler.createHandle("QLParser.call")
  lazy val processThing = Profiler.createHandle("QLParser.processThing")
  lazy val wikify = Profiler.createHandle("QLParser.wikify")
}
