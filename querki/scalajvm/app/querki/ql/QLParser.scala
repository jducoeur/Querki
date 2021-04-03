package querki.ql

import language.existentials

import scala.annotation.tailrec
import scala.util.parsing.combinator._

import models._

import querki.core.QLText
import querki.ecology._
import querki.globals._
import querki.html.QHtml
import querki.util.{DebugRenderable, UnexpectedPublicException}
import querki.values._

sealed trait QLParseResult[T <: QLParseResultVal]
case class QLParseSuccess[T <: QLParseResultVal](result: T) extends QLParseResult[T]
case class QLParseFailure[T <: QLParseResultVal](msgFut: Future[Wikitext]) extends QLParseResult[T]

class QLParser(
  val input: QLText,
  ci: QLContext,
  invOpt: Option[Invocation] = None,
  val lexicalThing: Option[PropertyBundle] = None,
  val lexicalProp: Option[AnyProp] = None,
  val initialBindings: Option[Map[String, QValue]] = None
)(implicit
  val ecology: Ecology
) extends RegexParsers
     with EcologyMember {

  // Add the parser to the context, so that methods can call back into it. Note that we are treating this as essentially
  // a modification, rather than another level of depth.
  val parserContext =
    ci.copy(parser = Some(this))(ci.state, ecology)

  /**
   * Create a new parser, that includes the given bindings. This is intended mainly for "meta" functions,
   * which want to fix some bindings and then call processExp().
   */
  def withBindings(bindings: Map[String, QValue]): QLParser = {
    new QLParser(input, ci, invOpt, lexicalThing, lexicalProp, Some(initialBindings.getOrElse(Map.empty) ++ bindings))
  }

  val paramsOpt = invOpt.flatMap(_.paramsOpt)

  lazy val QL = interface[querki.ql.QL]
  lazy val QLInternals = interface[QLInternals]

  lazy val qlProfilers = QLInternals.qlProfilers

  def WarningValue = QL.WarningValue _

  lazy val processor = new QLProcessor(parserContext, invOpt, Some(this), initialBindings)

  // *****************************************

  final val name = querki.core.nameRegex
  // These two regexes are used in the unQLText production. Yes, they *could* be combined into one
  // expression, and originally were. They were split because having them together apparently does
  // something horrible to Java's Regex engine -- if you tried to feed it more than a page or so of
  // matching text (that is, with no QL expressions), it would explode with a Stack Overflow error.
  // Splitting them seems to cure that, knock on wood.
  val unQLTextRegex = """[^\[\]\"_\\]+""".r
  val partialDelimiterRegex = """(\[(?!\[)|\"(?!\")|_(?!_)|\](?!\]))+""".r
  // We don't want the RegexParser removing whitespace on our behalf. Note that we need to be
  // very careful about whitespace!
  override val whiteSpace = "".r

  // Comments are defined in here. Note that a comment is defined as everything from "//"
  // to the end of the line *or* a "]]", so that we can put them at the end of an expression.
  def qlSpace: Parser[QLSpace] = "(\\s*(//([^\n\\]]|(\\]?!\\]))*)?\\s*)*".r ^^ { QLSpace(_) }

  def unQLText: Parser[UnQLText] =
    (unQLTextRegex |
      // You can use backslash to escape the "special" QL syntax delimiters:
      "\\\\".r ~> ("\"\"".r | "\\[\\[".r | "\\]\\]".r | "____".r | "__".r) |
      // If we haven't use the backslash for that, then just eat it as a normal character:
      "\\\\".r |
      partialDelimiterRegex) ^^ { UnQLText(_) }
  def qlNumber[QLNumber] = "\\s*".r ~> "\\-?\\d+".r ^^ { intStr => QLNumber(java.lang.Long.parseLong(intStr)) }
  def qlSafeName[QLSafeName] = name ^^ { QLSafeName(_) }
  def qlDisplayName[QLDisplayName] = "`" ~> "[^`]*".r <~ "`" ^^ { QLDisplayName(_) }
  def qlThingId[QLThingId] = "." ~> "\\w*".r ^^ { oid => QLThingId("." + oid) }
  def qlName: Parser[QLName] = qlDef | qlBindingDef | qlBinding | qlThingId | qlSafeName | qlDisplayName
  def qlOp: Parser[String] = ("&" | "|")
  def qlUnOp: Parser[String] = ("!")

  def qlCall: Parser[QLCall] = opt("\\*\\s*".r) ~ qlName ~ opt("." ~> qlName) ~ opt(qlParamList) ^^ {
    case collFlag ~ n ~ optMethod ~ optParams => QLCall(n, optMethod, optParams, collFlag)
  }

  // Note that the failure() here only works because we specifically disallow "]]" in a Text!
  def qlTextStage: Parser[QLTextStage] = (opt("\\*\\s*".r) <~ "\"\"") ~ qlText <~ ("\"\"" | failure(
    "Reached the end of the QL expression, but missing the closing \"\" for a Text expression in it"
  )) ^^ {
    case collFlag ~ text => QLTextStage(text, collFlag)
  }
  def qlExpStage: Parser[QLStage] = "(" ~> qlExp <~ ")" ^^ { QLExpStage(_) }

  def qlDef: Parser[QLBindingDef] = "\\s*_def\\s+\\$".r ~> name ~ opt(
    "(" ~> rep1sep("$" ~> name, "\\s*,\\s*".r) <~ ")"
  ) ~ ("\\s+=\\s+".r ~> qlPhrase) ^^ {
    case name ~ params ~ phrase => QLBindingDef(name, Some(phrase), params)
  }

  def qlTextBlockLiteral: Parser[QLTextBlockLiteral] = "\\s*".r ~ "```" ~> "(?s)(?:(?!```).)*".r <~ "```" ^^ {
    block => QLTextBlockLiteral(block)
  }

  def qlBinding: Parser[QLBinding] = "\\s*\\$".r ~> name ^^ {
    case name => QLBinding(name)
  }

  def qlBindingDef: Parser[QLBindingDef] =
    "\\s*\\+\\$".r ~> name ~ opt("\\(\\s*".r ~> (repsep(qlParam, "\\s*,\\s*".r) <~ "\\s*\\)".r)) ^^ {
      case name ~ params => QLBindingDef(name, None, None)
    }
  def qlBasicStage: Parser[QLStage] = qlTextBlockLiteral | qlNumber | qlCall | qlTextStage | qlList | qlExpStage

  def qlUnOpStage: Parser[QLStage] = opt(qlUnOp <~ "\\s*".r) ~ qlBasicStage ^^ {
    case unop ~ guts => {
      unop match {
        case Some(op) => {
          val funcName = op match {
            case "!" => "_not"
            case _   => throw new Exception(s"qlUnaryOperation got unknown operator $op")
          }
          QLCall(QLSafeName(funcName), None, Some(Seq(QLParam(None, QLExp(Seq(QLPhrase(Seq(guts))))))), None)
        }
        case None => guts
      }
    }
  }

  def qlStage: Parser[QLStage] = qlUnOpStage ~ opt("\\s+".r ~> qlOp ~ ("\\s+".r ~> qlUnOpStage)) ^^ {
    case left ~ operation => {
      operation match {
        case Some(op ~ right) => {
          val funcName = op match {
            case "&" => "_and"
            case "|" => "_or"
            case _   => throw new Exception(s"qlBinaryOperation got unknown operator $op")
          }
          QLCall(
            QLSafeName(funcName),
            None,
            Some(Seq(QLParam(None, QLExp(Seq(QLPhrase(Seq(left))))), QLParam(None, QLExp(Seq(QLPhrase(Seq(right))))))),
            None
          )
        }
        case None => left
      }
    }
  }
  def qlParamList: Parser[Seq[QLParam]] = qlEmptyParamList | qlRealParamList
  def qlEmptyParamList: Parser[Seq[QLParam]] = "\\(\\s*\\)".r ^^ { case _ => Seq.empty }
  def qlRealParamList: Parser[Seq[QLParam]] = "\\(\\s*".r ~> (rep1sep(qlParam, "\\s*,\\s*".r) <~ "\\s*\\)".r)

  def qlParam: Parser[QLParam] = opt(name <~ "\\s*=\\s*".r) ~ opt("~!") ~ qlExp ^^ {
    case nameOpt ~ immediateOpt ~ exp => QLParam(nameOpt, exp, immediateOpt.isDefined)
  }
  def qlPhrase: Parser[QLPhrase] = rep1sep(qlStage, qlSpace ~ "->".r ~ qlSpace) ^^ { QLPhrase(_) }

  // Phrases are separated either by qlSpace (including comments) or semicolons
  def qlExp: Parser[QLExp] = opt(qlSpace) ~> repsep(qlPhrase, opt(qlSpace) ~ opt(";") ~ qlSpace) <~ opt(qlSpace) ^^ {
    QLExp(_)
  }
  def qlLink: Parser[QLLink] = qlText ^^ { QLLink(_) }

  def qlText: Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]" | "__" ~> qlLink <~ ("__" | failure(
    "Underscores must always be in pairs or sets of four"
  ))) ^^ {
    ParsedQLText(_)
  }

  def qlList: Parser[QLCall] = "<" ~> rep1sep(qlExp, qlSpace ~ "," ~ qlSpace) <~ qlSpace ~ ">" ^^
    // We've found that _concat already works well for this, so simply desugar to that:
    // { QLListLiteral(_) }
    { params => QLCall(QLSafeName("_concat"), None, Some(params.map(param => QLParam(None, param, false))), None) }

  /**
   * Wrapper around all text parsing. Deals with both profiling and memoizing the result.
   */
  private def parseMemoized[T <: QLParseResultVal](parser: Parser[T]): QLParseResult[T] = {
    qlProfilers.parse.profile {
      input.memoizedParse {
        val result = parseAll(parser, input.text)
        result match {
          case Success(result, _) => QLParseSuccess(result)
          case Failure(msg, next) => QLParseFailure(renderError(msg, next))
          // TODO: we should probably do something more serious in case of Error:
          case Error(msg, next) =>
            QLParseFailure { QLog.error("Couldn't parse qlText: " + msg); renderError(msg, next) }
        }
      }
    }
  }

  def parse = parseMemoized(qlText)

  def consumeReader[T](reader: scala.util.parsing.input.Reader[T]): String = {
    reader.first.toString + (if (reader.atEnd) "" else consumeReader(reader.rest))
  }

  // TODO: this really shouldn't be showing raw HTML. Redo this properly as Wikitext:
  def renderError(
    msg: String,
    reader: scala.util.parsing.input.Reader[_]
  ): Future[Wikitext] = {
    val pos = reader.pos
    val propStr = parserContext.getProp match {
      case Some(prop) => " " + prop.displayName + ", "
      case None       => ""
    }
    val escapedMsg = s"<b>Syntax error in $propStr line ${pos.line}:</b> " + scala.xml.Utility.escape(msg)
    val escapedError = scala.xml.Utility.escape(pos.longString)
    Future.successful(HtmlWikitext(QHtml(
      "<p>" + escapedMsg + ":<p>\n" +
        "<pre>" + escapedError + "</pre>\n"
    )))
  }

  def wikiFut(str: String): Future[Wikitext] =
    Future.successful(Wikitext(str))

  def process: Future[Wikitext] = {
    try {
      val parseResult = parse
      parseResult match {
        case QLParseSuccess(result) => {
          val context = processor.initialContext
          processor.processParseTree(result, context)
            .recoverWith {
              case ex: PublicException => WarningValue(ex.display(context.requestOpt)).wikify(context)
            }
        }
        case QLParseFailure(msgFut) => msgFut
      }
    } catch {
      case overflow: java.lang.StackOverflowError => {
        QLog.error("Stack overflow error while trying to parse this QLText:\n" + input.text)
        overflow.printStackTrace()
        wikiFut(
          "We're sorry -- this thing is apparently more complex than Querki can currently cope with. Please contact Justin: this is a bug we need to fix."
        )
      }
      case error: Exception => {
        QLog.error("Exception during QL Processing: " + error, error)
        wikiFut("We're sorry -- there was an error while trying to display this thing.")
      }
      case error: Throwable => {
        QLog.error("Throwable during QL Processing: " + error, error)
        wikiFut("We're sorry -- there was a serious error while trying to display this thing.")
      }
    }
  }

  def parsePhrase(): Option[QLPhrase] = {
    val parseResult = parseMemoized(qlPhrase)
    parseResult match {
      case QLParseSuccess(result) => Some(result)
      case _                      => None
    }
  }

  private[ql] def processMethodToWikitext: Future[Wikitext] = {
    val parseResult = parseMemoized(qlExp)
    parseResult match {
      case QLParseSuccess(result) =>
        qlProfilers.processMethod.profile {
          processor.contextsToWikitext(processor.processExpAll(result, processor.initialContext, false), false)
        }
      case QLParseFailure(msgFut) => {
        msgFut.flatMap(err => parserContext.next(QL.WikitextValue(err)).value.wikify(parserContext))
      }
    }
  }

  private[ql] def processMethod: Future[QLContext] = {
    val parseResult = parseMemoized(qlExp)
    parseResult match {
      case QLParseSuccess(result) =>
        qlProfilers.processMethod.profile {
          val context = processor.initialContext
          processor.processExp(result, context, false)
            .recoverWith {
              case ex: PublicException => processor.warningFut(context, ex.display(context.requestOpt))
            }
        }
      case QLParseFailure(msgFut) => { msgFut.map(err => parserContext.next(QL.WikitextValue(err))) }
    }
  }

  def processExp(
    exp: QLExp,
    context: QLContext,
    isParam: Boolean = false
  ): Future[QLContext] = processor.processExp(exp, context, isParam)

  def processExpAsScope(
    exp: QLExp,
    context: QLContext
  ): Future[QLContext] = {
    processor.withScope(context, processExp(exp, _))
  }

}
