package ql

import scala.util.parsing.combinator._

class QLTextPart
case class UnQLText(text:String) extends QLTextPart
case class QLName(name:String)
case class QLPhrase(ops:Seq[QLName])
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart
case class QLText(parts:Seq[QLTextPart])

class QLParser extends RegexParsers {
  val name = """[a-zA-Z][\w- ]*""".r
  val unQLTextRegex = """([^\[]|\[(?!\[))+""".r
  
  def unQLText:Parser[UnQLText] = unQLTextRegex ^^ { UnQLText(_) }
  def qlPhrase:Parser[QLPhrase] = name ^^ { n => QLPhrase(Seq(QLName(n))) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\n") ^^ { QLExp(_) }
  def qlText:Parser[QLText] = rep(unQLText | "[[" ~> qlExp <~ "]]") ^^ { QLText(_) }
}