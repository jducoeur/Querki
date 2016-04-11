package querki.ql

import querki.values.QLContext

// **************************************************
//
// QL Abstract Syntax Tree
//
// The following case classes define the syntax tree for a QLText block.
//
// Only a few bits of this are of interest to the outside world, and even those shouldn't
// be -- they should be hidden behind a better abstraction.
//
// **************************************************

case class QLNumber(n:Int) extends QLStage(None) {
  def reconstructString = n.toString
}

case class QLPhrase(ops:Seq[QLStage]) {
  def reconstructString = ops.map(_.reconstructString).mkString(" -> ")
}

case class QLParam(name:Option[String], exp:QLExp, immediate:Boolean = false, resolved:Option[QLContext] = None) {
  def reconstructString = s"${name.map(_ + " = ").getOrElse("")}${exp.reconstructStandalone}"
  def firstOps = exp.phrases.head.ops
  def isNamed = name.isDefined
}

case class QLCall(name:QLName, methodName:Option[QLName], params:Option[Seq[QLParam]], collFlag:Option[String]) extends QLStage(collFlag) {
  def reconstructString = collFlag.getOrElse("") +
    name.reconstructString +
    methodName.map(method => "." + method.reconstructString).getOrElse("") +
    params.map("(" + _.map(_.reconstructString).mkString(", ") + ")").getOrElse("")
}

private[ql] sealed abstract class QLName(val name:String) {
  def reconstructString:String
}
private[ql] case class QLSafeName(n:String) extends QLName(n) {
  def reconstructString:String = n
}
private[ql] case class QLDisplayName(n:String) extends QLName(n) {
  def reconstructString:String = "`" + n + "`"
}
private[ql] case class QLBinding(n:String, assign:Boolean = false, func:Option[QLPhrase] = None) extends QLName(n) {
  def reconstructString = {
    func match {
      case Some(f) => s"_def $$$n = ${f.reconstructString}"
      case _ => (if (assign) "+" else "") + "$" + n
    }
  }
}
private[ql] case class QLThingId(n:String) extends QLName(n) {
  def reconstructString = n
}
private[ql] sealed abstract class QLTextPart {
  def reconstructString:String
  
  override def toString = reconstructString
}
private[ql] case class UnQLText(text:String) extends QLTextPart {
  def reconstructString:String = text
}
private[ql] sealed abstract class QLStage(collFlag:Option[String]) {
  def reconstructString:String
  def useCollection:Boolean = collFlag match {
    case Some(_) => true
    case None => false
  }
  def clearUseCollection:Boolean = false
  
  override def toString = reconstructString
}
private[ql] case class QLTextStage(contents:ParsedQLText, collFlag:Option[String]) extends QLStage(collFlag) {
  def reconstructString = collFlag.getOrElse("") + "\"\"" + contents.reconstructString + "\"\""
  
  override def clearUseCollection = collFlag.isEmpty
}
case class QLExpStage(exp:QLExp) extends QLStage(None) {
  def reconstructString = "(" + exp.reconstructString + ")"
}
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart {
  def reconstructStandalone = phrases.map(_.reconstructString).mkString("\n")
  def reconstructString = "[[" + reconstructStandalone + "]]"
}
private[ql] case class QLLink(contents:ParsedQLText) extends QLTextPart {
  def reconstructString = "__" + contents.reconstructString + "__"
}
private[ql] case class ParsedQLText(parts:Seq[QLTextPart]) {
  def reconstructString = parts.map(_.reconstructString).mkString
}
private[ql] case class QLSpace(text:String)

private[ql] case class QLListLiteral(exps:Seq[QLExp]) extends QLStage(None) {
  def reconstructString = "<" + exps.map(_.reconstructStandalone).mkString(", ") + ">"
}
