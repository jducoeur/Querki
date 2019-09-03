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

// Marker trait for the types that can be results of parsing a complete QLText.
sealed trait QLParseResultVal

case class QLNumber(n:Long) extends QLStage(None) {
  def reconstructString = n.toString
}

case class QLPhrase(ops:Seq[QLStage]) extends QLParseResultVal {
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
private[ql] case class QLBinding(n:String) extends QLName(n) {
  def reconstructString = {
    "$" + n
  }
}
private[ql] case class QLBindingDef(n:String, func:Option[QLPhrase] = None, params:Option[Seq[String]] = None) extends QLName(n) {
  def reconstructString = {
    func match {
      case Some(f) => s"_def $$$n${params.map(_.map("$" + _).mkString("(", ", ", ")")).getOrElse("")} = ${f.reconstructString}"
      case _ => "+$" + n
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
case class UnQLText(text:String) extends QLTextPart {
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
case class QLTextStage(contents:ParsedQLText, collFlag:Option[String]) extends QLStage(collFlag) {
  def reconstructString = collFlag.getOrElse("") + "\"\"" + contents.reconstructString + "\"\""
  
  override def clearUseCollection = collFlag.isEmpty
}
case class QLExpStage(exp:QLExp) extends QLStage(None) {
  def reconstructString = "(" + exp.reconstructString + ")"
}
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart with QLParseResultVal {
  def reconstructStandalone = phrases.map(_.reconstructString).mkString("\n")
  def reconstructString = "[[" + reconstructStandalone + "]]"
}
private[ql] case class QLLink(contents:ParsedQLText) extends QLTextPart {
  def reconstructString = "__" + contents.reconstructString + "__"
}
case class ParsedQLText(parts:Seq[QLTextPart]) extends QLParseResultVal {
  def reconstructString = parts.map(_.reconstructString).mkString
}
private[ql] case class QLSpace(text:String)

private[ql] case class QLListLiteral(exps:Seq[QLExp]) extends QLStage(None) {
  def reconstructString = "<" + exps.map(_.reconstructStandalone).mkString(", ") + ">"
}

private[ql] case class QLTextBlockLiteral(text: String) extends QLStage(None) {
  def reconstructString = "```\n" + text + "\n```" 
}
  
/**
 * Represents a Closure, more or less -- a QL Expression that is wrapped up so that it can be
 * evaluated later. Doesn't yet exist at the user level, but we use it internally for bound local functions.
 * 
 * For now, we aren't capturing the Scopes as of the point of definition. This potentially allows the
 * function to refer to values that are bound after its definition but before the call site. Do we care?
 * 
 * @param exp The guts of the local function.
 * @param params The formal parameters of this function.
 */
case class QLClosure(exp:QLExp, params:Option[Seq[String]])
