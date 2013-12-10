package ql

import language.existentials

import scala.util.parsing.combinator._

import models.system._
import models._

import querki.util._
import querki.values._

// TODO: this should go away
import play.api.Logger

/**
 * A QLFunction is something that can be called in the QL pipeline. It is mostly implemented
 * by Thing (with variants for Property and suchlike), but can also be anonymous -- for example,
 * when a method returns a partially-applied function.
 */
trait QLFunction {
  def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue
}

class PartiallyAppliedFunction(partialContext:QLContext, action:(QLContext, Option[Seq[QLPhrase]]) => QValue) extends QLFunction {
  def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
    action(context, params)
  }
}

class BogusFunction extends QLFunction {
  def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
    ErrorValue("It does not make sense to put this after a dot.")
  }
}

sealed abstract class QLTextPart {
  def reconstructString:String
  
  override def toString = reconstructString
}
case class UnQLText(text:String) extends QLTextPart {
  def reconstructString:String = text
}
sealed abstract class QLStage(collFlag:Option[String]) {
  def reconstructString:String
  def useCollection:Boolean = collFlag match {
    case Some(_) => true
    case None => false
  }
  
  override def toString = reconstructString
}
case class QLCall(name:String, methodName:Option[String], params:Option[Seq[QLPhrase]], collFlag:Option[String]) extends QLStage(collFlag) {
  def reconstructString = collFlag.getOrElse("") +
    name +
    methodName.map(str => "." + str).getOrElse("") +
    params.map("(" + _.map(_.reconstructString).mkString(", ") + ")").getOrElse("")
}
case class QLTextStage(contents:ParsedQLText, collFlag:Option[String]) extends QLStage(collFlag) {
  def reconstructString = collFlag.getOrElse("") + "\"\"" + contents.reconstructString + "\"\""
}
case class QLBinding(name:String) extends QLStage(None) {
  def reconstructString = "$" + name
}
case class QLPhrase(ops:Seq[QLStage]) {
  def reconstructString = ops.map(_.reconstructString).mkString(" -> ")
}
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart {
  def reconstructString = "[[" + phrases.map(_.reconstructString).mkString + "]]"
}
case class QLLink(contents:ParsedQLText) extends QLTextPart {
  def reconstructString = "__" + contents.reconstructString + "__"
}
case class ParsedQLText(parts:Seq[QLTextPart]) {
  def reconstructString = parts.map(_.reconstructString).mkString
}

class QLParser(val input:QLText, ci:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None) extends RegexParsers {
  
  // Add the parser to the context, so that methods can call back into it. Note that we are treating this as essentially
  // a modification, rather than another level of depth:
  val initialContext = ci.copy(parser = Some(this))
  
  // Crude but useful debugging of the process tree. Could stand to be beefed up when I have time
  // Note that this is fundamentally not threadsafe, because of logDepth!
  // Note that the indentation doesn't work across multiple QLParsers!
  // TODO: we could make indentation work across parsers (and fix thread-safety) if we stuffed the indentation level into the
  // Context, and had logContext actually transform the context. This may imply that we want to make
  // this mechanism more broadly available -- possibly even a method on the context itself...
  // TODO: this mechanism can, and probably should, be removed from here and moved into QLog. Context has
  // become central enough to the system that making it more globally visible would be a win. If we remove
  // the constraint that RT must derive from DebugRenderable (make that optional and test with isInstanceOf),
  // then this could be used everywhere, to very good effect. It would become an enormously powerful (if
  // nearly overwhelming) spew for tracking the passage of a parse through the system.
  var logDepth = 0
  def indent = "  " * logDepth
  // Turn this to true to produce voluminous output from the QL processing pipeline
  val doLogContext = Config.getBoolean("querki.test.logContexts", false)
  def logContext[RT <: DebugRenderable](msg:String, context:QLContext)(processor: => RT):RT = {
    if (doLogContext) {
      Logger.info(indent + msg + ": " + context.debugRender + " =")
      Logger.info(indent + "{")
      logDepth = logDepth + 1
    }
    val result = processor
    if (doLogContext) {
      logDepth = logDepth - 1
      
      Logger.info(indent + "} = " + result.debugRender)
    }
    result
  }
  
  // Note that we allow *one* slash in a name, but not pairs:
  val name = """[a-zA-Z_]([\w-_ ]|/?!/)*[\w]""".r
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
  
  def unQLText:Parser[UnQLText] = (unQLTextRegex | 
      // You can use backslash to escape the "special" QL syntax delimiters:
      "\\\\".r ~> ("\"\"".r | "\\[\\[".r | "\\]\\]".r | "____".r | "__".r) |
      // If we haven't use the backslash for that, then just eat it as a normal character:
      "\\\\".r | 
      partialDelimiterRegex) ^^ { UnQLText(_) }
  def qlCall:Parser[QLCall] = opt("\\*\\s*".r) ~ name ~ opt("." ~> name) ~ opt("\\(\\s*".r ~> (rep1sep(qlPhrase, "\\s*,\\s*".r) <~ "\\s*\\)".r)) ^^ { 
    case collFlag ~ n ~ optMethod ~ optParams => QLCall(n, optMethod, optParams, collFlag) }
  // Note that the failure() here only works because we specifically disallow "]]" in a Text!
  def qlTextStage:Parser[QLTextStage] = (opt("\\*\\s*".r) <~ "\"\"") ~ qlText <~ ("\"\"" | failure("Reached the end of the QL expression, but missing the closing \"\" for a Text expression in it") ) ^^ {
    case collFlag ~ text => QLTextStage(text, collFlag) }
  def qlBinding:Parser[QLBinding] = "\\s*\\$".r ~> name ^^ { QLBinding(_) } 
  def qlStage:Parser[QLStage] = qlBinding | qlCall | qlTextStage
  // TODO: phrase is going to get a *lot* more complex with time:
  def qlPhrase:Parser[QLPhrase] = rep1sep(qlStage, "(\\s*(//[^\n]*)?\\s*)*->(\\s*(//[^\n]*)?\\s*)*".r) ^^ { QLPhrase(_) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\\s*\\r?\\n|\\s*;\\s*".r) ^^ { QLExp(_) }
  def qlLink:Parser[QLLink] = qlText ^^ { QLLink(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]" | "__" ~> qlLink <~ ("__" | failure("Underscores must always be in pairs or sets of four"))) ^^ { 
    ParsedQLText(_) }
  
  /**
   * Note that the output here is nominally a new Context, but the underlying type is
   * ParsedTextType. So far, you can't *do* anything with that afterwards other than
   * render it, which just returns the already-computed Wikitext.
   */
  private def processTextStage(text:QLTextStage, context:QLContext):QLContext = {
    logContext("processTextStage " + text, context) {
	    val ct = context.value.cType
	    // For each element of the incoming context, recurse in and process the embedded Text
	    // in that context.
	    val transformed = context.map { elemContext =>
	      ParsedTextType(processParseTree(text.contents, elemContext))
	    }
	    // TBD: the asInstanceOf here is surprising -- I would have expected transformed to come out
	    // as the right type simply by type signature. Can we get rid of it?
	    context.next(ct.makePropValue(transformed.asInstanceOf[ct.implType], ParsedTextType))
    }
  }
  
  private def processCall(call:QLCall, context:QLContext):QLContext = {
    logContext("processName " + call, context) {
	    val thing = context.state.anythingByName(call.name)
	    val tv = thing match {
	      case Some(t) => {
	        // If there are parameters to the call, they are a collection of phrases.
	        val params = call.params
	        val methodOpt = call.methodName.flatMap(context.state.anythingByName(_))
	        try {
	          methodOpt match {
	            case Some(method) => {
	              val partialFunction = method.partiallyApply(context.next(ExactlyOne(LinkType(t.id))))
	              partialFunction.qlApply(context, params)
	            }
	            case None => t.qlApply(context, params)
	          }
	        } catch {
	          case ex:PublicException => WarningValue(ex.display(context.requestOpt))
	          case error:Exception => QLog.error("Error during QL Processing", error); WarningValue(UnexpectedPublicException.display(context.requestOpt))
	        }
	      }
	      // They specified a name we don't know. Turn it into a raw NameType and pass it through. If it gets to
	      // the renderer, it will turn into a link to the undefined page, where they can create it.
	      //
	      // TBD: in principle, we might want to make this more Space-controllable. But it isn't obvious that we care. 
	      case None => ExactlyOne(UnknownNameType(call.name))
	    }
	    context.next(tv)
    }
  }
  
  private def processInternalBinding(binding:QLBinding, context:QLContext):QLContext = {
    if (binding.name == "_context") {
      initialContext
    } else {
      try {
        val rawParamNum = Integer.parseInt(binding.name.substring(1))
        val paramNum = rawParamNum - 1
        val resultOpt = for (
          params <- paramsOpt;
          phrase = params(paramNum)
            )
          yield processPhrase(phrase.ops, context)
        
        resultOpt.getOrElse(context.next(WarningValue("No parameters passed in")))
      } catch {
        case ex:NumberFormatException => context.next(WarningValue("$" + binding.name + " is not a valid bound name"))
        case ex:IndexOutOfBoundsException => context.next(WarningValue("Not enough parameters passed in"))
      }
    }
  }
  
  private def processBinding(binding:QLBinding, context:QLContext):QLContext = {
    logContext("processBinding " + binding, context) {
      if (binding.name.startsWith("_"))
        processInternalBinding(binding, context)
      else
        context.next(WarningValue("Only internal bindings, starting with $_, are allowed at the moment."))
    }
  }
  
  private def processStage(stage:QLStage, contextIn:QLContext):QLContext = {
    val context = 
      if (contextIn.depth > contextIn.maxDepth)
        contextIn.next(WarningValue("Too many levels of calls -- you can only have up to " + contextIn.maxDepth + " calls in a phrase."));
      else if (stage.useCollection) 
        contextIn.asCollection 
      else 
        contextIn
    logContext("processStage " + stage, context) {
	    stage match {
	      case name:QLCall => processCall(name, context)
	      case subText:QLTextStage => processTextStage(subText, context)
	      case binding:QLBinding => processBinding(binding, context)
//        case QLPlainTextStage(text) => context.next(TextValue(text))
	    }
    }
  }
  
  def processPhrase(ops:Seq[QLStage], startContext:QLContext):QLContext = {
    logContext("processPhrase " + ops, startContext) {
	    (startContext /: ops) { (context, stage) => 
	      if (context.isCut)
	        // Setting the "cut" flag means that we're just stopping processing here. Usually
	        // used for warnings:
	        context
	      else
	        processStage(stage, context) 
	    }
    }
  }
  
  private def processPhrases(phrases:Seq[QLPhrase], context:QLContext):Seq[QLContext] = {
    phrases map (phrase => processPhrase(phrase.ops, context))
  }

  def contextsToWikitext(contexts:Seq[QLContext], insertNewlines:Boolean = false):Wikitext = {
    (Wikitext("") /: contexts) { (soFar, context) => soFar.+(context.value.wikify(context.parent), insertNewlines) }
  }
  
  /**
   * This deals with QText that contains "__stuff__" or "____", both of which render as
   * the value of the received context.
   * 
   * The key notion here is that the wikify() method takes an optional "displayOpt" parameter.
   * If specified, this says how to display the passed-in value. It doesn't make sense for all
   * Types, but it is always *syntactically* legal, and is provided as a tool for any Type that
   * wants to avail itself of this notion.
   */
  private def linkToWikitext(contents:ParsedQLText, context:QLContext):Wikitext = {
    logContext("linkToWikitext " + contents, context) {
      val guts = contents.parts.length match {
        // Just four underscores, which means we don't do anything special for the display:
        case 0 => None
        // There is content, so pass that down as the display:
        case _ => Some(processParseTree(contents, context))
      }
      context.value.wikify(context, guts)
    }
  }
  
  private def processParseTree(parseTree:ParsedQLText, context:QLContext):Wikitext = {
    (Wikitext("") /: parseTree.parts) { (soFar, nextPart) =>
      soFar + (nextPart match {
        case UnQLText(t) => Wikitext(t)
        case QLExp(phrases) => contextsToWikitext(processPhrases(phrases, context))
        case QLLink(l) => linkToWikitext(l, context)
      })
    }
  }
  
  def parse = parseAll(qlText, input.text)
  
  def consumeReader[T](reader:scala.util.parsing.input.Reader[T]):String = {
    reader.first.toString + (if (reader.atEnd) "" else consumeReader(reader.rest))
  }
  
  // TODO: this really shouldn't be showing raw HTML. Redo this properly as Wikitext:
  def renderError(msg:String, reader:scala.util.parsing.input.Reader[_]):Wikitext = {
    val pos = reader.pos
    val propStr = initialContext.getProp match {
      case Some(prop) => " " + prop.displayName + ", "
      case None => ""
    }
    val escapedMsg = s"<b>Syntax error in $propStr line ${pos.line}:</b> " + scala.xml.Utility.escape(msg)
    val escapedError = scala.xml.Utility.escape(pos.longString)
    HtmlWikitext(play.api.templates.Html(
        "<p>" + escapedMsg + ":<p>\n" +
        "<pre>" + escapedError + "</pre>\n"))
  }
  
  def process:Wikitext = {
    try {
      val parseResult = parse
      parseResult match {
        case Success(result, _) => processParseTree(result, initialContext)
        case Failure(msg, next) => { renderError(msg, next) }
        // TODO: we should probably do something more serious in case of Error:
        case Error(msg, next) => { Logger.error("Couldn't parse qlText: " + msg); renderError(msg, next) }
      }
    } catch {
      case overflow:java.lang.StackOverflowError => {
        Logger.error("Stack overflow error while trying to parse this QLText:\n" + input.text)
        overflow.printStackTrace()
        Wikitext("We're sorry -- this thing is apparently more complex than Querki can currently cope with. Please contact Justin: this is a bug we need to fix.")
      }
      case error:Exception => {
        Logger.error("Error during QL Processing: " + error, error)
        Wikitext("We're sorry -- there was an error while trying to display this thing.")
      }
      case error:Throwable => {
        Logger.error("Error during QL Processing: " + error)
        Wikitext("We're sorry -- there was an error while trying to display this thing.")
      }
    }
  }
  
  def processMethod:QLContext = {
    val parseResult = parseAll(qlPhrase, input.text)
    parseResult match {
      case Success(result, _) => processPhrase(result.ops, initialContext)
      case Failure(msg, next) => { initialContext.next(WikitextValue(renderError(msg, next))) }
      case Error(msg, next) => { initialContext.next(WikitextValue(renderError(msg, next))) }
    }
  }
}